%%------------------------------------------------------------------------------
%% @doc 固定大小的环形缓冲区（链表实现）
%%
%% 本模块实现了一个固定大小的缓冲区，用于存储 uTP 协议的发送/接收数据包。
%% 内部使用两个数组：一个存储数据，一个存储链表索引。
%%
%% == 数据结构 ==
%% ```
%% +------------------+     +------------------+
%% |   data array     |     |   index array    |
%% +------------------+     +------------------+
%% | 0: packet_a      |     | 0: 1 (next=1)    |
%% | 1: packet_b      |     | 1: 2 (next=2)    |
%% | 2: packet_c      |     | 2: -1 (end)      |
%% | 3: undefined     |     | 3: 4 (free list) |
%% | 4: undefined     |     | 4: -1 (end)      |
%% +------------------+     +------------------+
%%
%% used (head) = 0  -->  0 -> 1 -> 2 -> -1 (已用链表)
%% free        = 3  -->  3 -> 4 -> -1      (空闲链表)
%% tail        = 2                         (已用链表尾)
%% '''
%%
%% == 设计原理 ==
%% - 使用链表而非环形索引，支持任意位置的插入和删除
%% - 空闲槽位也形成链表，删除后的槽位可被重用
%% - O(1) 的头部/尾部操作，O(n) 的中间插入/删除
%%
%% == 主要用途 ==
%% - outbuf: 发送缓冲区，存储待发送和待确认的数据包
%% - inbuf: 接收缓冲区，存储乱序到达的数据包（重排序缓冲区）
%%
%% @author David Gao <david.alpha.fox@gmail.com>
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_buffer).

%%==============================================================================
%% API 导出
%%==============================================================================
-export([
    new/1,          %% 创建新缓冲区
    head/1,         %% 获取头部索引
    tail/1,         %% 获取尾部索引
    next/2,         %% 获取下一个索引
    data/2,         %% 获取指定位置的数据
    replace/3,      %% 替换指定位置的数据
    delete/3,       %% 删除指定位置的元素
    insert/3,       %% 在指定位置后插入元素
    append/2,       %% 追加到尾部
    pop/1,          %% 弹出头部元素
    size/1,         %% 获取缓冲区总大小
    used/1,         %% 获取已用槽位数
    unused/1        %% 获取空闲槽位数
]).

-export_type([aiutp_buffer/0, buffer_index/0]).

%%==============================================================================
%% 常量定义
%%==============================================================================

%% 表示链表结束或无效索引
-define(END_INDEX, -1).

%%==============================================================================
%% 类型定义
%%==============================================================================

%% 缓冲区索引类型：-1 表示结束/无效，非负整数表示有效索引
-type buffer_index() :: -1 | non_neg_integer().

%% 缓冲区记录
-record(aiutp_buffer, {
    data :: array:array(),              %% 数据数组
    index :: array:array(),             %% 链表索引数组
    used = ?END_INDEX :: buffer_index(),%% 已用链表头（head）
    free = 0 :: buffer_index(),         %% 空闲链表头
    size = 0 :: non_neg_integer(),      %% 缓冲区总大小
    unused = 0 :: non_neg_integer(),    %% 空闲槽位数
    tail = ?END_INDEX :: buffer_index() %% 已用链表尾
}).

-opaque aiutp_buffer() :: #aiutp_buffer{}.

%%==============================================================================
%% API 函数 - 创建
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc 创建指定大小的新缓冲区
%%
%% 初始化数据数组和索引数组，所有槽位都在空闲链表中。
%%
%% @param Size 缓冲区大小（必须为正整数）
%% @returns 新的空缓冲区
%% @end
%%------------------------------------------------------------------------------
-spec new(pos_integer()) -> aiutp_buffer().
new(Size) ->
    Data = array:new(Size, {fixed, true}),
    Index = array:new(Size, [{default, 0}, {fixed, true}]),
    init(#aiutp_buffer{data = Data, index = Index,
                       size = Size, unused = Size}).

%%==============================================================================
%% API 函数 - 查询
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc 获取缓冲区总大小
%% @end
%%------------------------------------------------------------------------------
-spec size(aiutp_buffer()) -> non_neg_integer().
size(#aiutp_buffer{size = Size}) -> Size.

%%------------------------------------------------------------------------------
%% @doc 获取已用槽位数
%% @end
%%------------------------------------------------------------------------------
-spec used(aiutp_buffer()) -> non_neg_integer().
used(#aiutp_buffer{size = Size, unused = Unused}) -> Size - Unused.

%%------------------------------------------------------------------------------
%% @doc 获取空闲槽位数
%% @end
%%------------------------------------------------------------------------------
-spec unused(aiutp_buffer()) -> non_neg_integer().
unused(#aiutp_buffer{unused = Unused}) -> Unused.

%%------------------------------------------------------------------------------
%% @doc 获取已用链表的头部索引
%%
%% 返回 -1 表示缓冲区为空。
%% @end
%%------------------------------------------------------------------------------
-spec head(aiutp_buffer()) -> buffer_index().
head(#aiutp_buffer{used = Used}) -> Used.

%%------------------------------------------------------------------------------
%% @doc 获取已用链表的尾部索引
%%
%% 返回 -1 表示缓冲区为空。
%% @end
%%------------------------------------------------------------------------------
-spec tail(aiutp_buffer()) -> buffer_index().
tail(#aiutp_buffer{tail = Tail}) -> Tail.

%%------------------------------------------------------------------------------
%% @doc 获取链表中的下一个索引
%%
%% @param Pos 当前位置
%% @param Buffer 缓冲区
%% @returns 下一个位置的索引，-1 表示链表结束
%% @end
%%------------------------------------------------------------------------------
-spec next(non_neg_integer(), aiutp_buffer()) -> buffer_index().
next(Pos, #aiutp_buffer{index = Index}) ->
    array:get(Pos, Index).

%%------------------------------------------------------------------------------
%% @doc 获取指定位置的数据
%%
%% @param Pos 数据位置
%% @param Buffer 缓冲区
%% @returns 该位置存储的数据
%% @end
%%------------------------------------------------------------------------------
-spec data(non_neg_integer(), aiutp_buffer()) -> term().
data(Pos, #aiutp_buffer{data = Data}) ->
    array:get(Pos, Data).

%%==============================================================================
%% API 函数 - 修改
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc 替换指定位置的数据
%%
%% 不改变链表结构，仅更新数据。
%%
%% @param Pos 要替换的位置
%% @param Val 新值
%% @param Buffer 缓冲区
%% @returns 更新后的缓冲区
%% @end
%%------------------------------------------------------------------------------
-spec replace(non_neg_integer(), term(), aiutp_buffer()) -> aiutp_buffer().
replace(Pos, Val, #aiutp_buffer{data = Data} = Buffer) ->
    Data2 = array:set(Pos, Val, Data),
    Buffer#aiutp_buffer{data = Data2}.

%%------------------------------------------------------------------------------
%% @doc 在指定位置后插入新元素
%%
%% - Prev = -1: 插入到链表头部
%% - Prev = 其他: 插入到 Prev 之后
%%
%% @param Prev 插入位置的前一个元素索引
%% @param Val 要插入的值
%% @param Buffer 缓冲区
%% @returns 更新后的缓冲区，或 {error, buffer_overflow}
%% @end
%%------------------------------------------------------------------------------
-spec insert(buffer_index(), term(), aiutp_buffer()) ->
    aiutp_buffer() | {error, buffer_overflow}.
insert(Prev, Val, Buffer) ->
    case alloc(Buffer) of
        {_, ?END_INDEX} ->
            {error, buffer_overflow};
        {Buffer2, Pos} ->
            do_insert(Prev, Pos, Val, Buffer2)
    end.

%%------------------------------------------------------------------------------
%% @doc 追加元素到缓冲区尾部
%%
%% 等价于 insert(tail(Buffer), Val, Buffer)。
%%
%% @param Val 要追加的值
%% @param Buffer 缓冲区
%% @returns 更新后的缓冲区，或 {error, buffer_overflow}
%% @end
%%------------------------------------------------------------------------------
-spec append(term(), aiutp_buffer()) -> aiutp_buffer() | {error, buffer_overflow}.
append(Val, Buffer) ->
    insert(Buffer#aiutp_buffer.tail, Val, Buffer).

%%------------------------------------------------------------------------------
%% @doc 删除指定位置的元素
%%
%% @param Pos 要删除的元素位置
%% @param Prev 被删除元素的前一个元素索引（-1 表示删除头部）
%% @param Buffer 缓冲区
%% @returns 更新后的缓冲区
%% @end
%%------------------------------------------------------------------------------
-spec delete(non_neg_integer(), buffer_index(), aiutp_buffer()) -> aiutp_buffer().
delete(Pos, ?END_INDEX, Buffer) ->
    %% 删除头部元素
    delete_head(Pos, Buffer);
delete(Pos, Prev, Buffer) ->
    %% 删除中间或尾部元素
    delete_after(Pos, Prev, Buffer).

%%------------------------------------------------------------------------------
%% @doc 弹出头部元素
%%
%% 等价于 delete(head(Buffer), -1, Buffer)。
%%
%% @param Buffer 缓冲区
%% @returns 更新后的缓冲区
%% @end
%%------------------------------------------------------------------------------
-spec pop(aiutp_buffer()) -> aiutp_buffer().
pop(Buffer) ->
    delete(Buffer#aiutp_buffer.used, ?END_INDEX, Buffer).

%%==============================================================================
%% 内部函数 - 初始化
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 初始化缓冲区的索引数组
%%
%% 将所有槽位串成空闲链表：0 -> 1 -> 2 -> ... -> Size-1 -> -1
%%------------------------------------------------------------------------------
-spec init(aiutp_buffer()) -> aiutp_buffer().
init(#aiutp_buffer{index = Index} = Buffer) ->
    LastIndex = array:size(Index) - 1,
    Index2 = init_free_list(Index, 0, LastIndex),
    Buffer#aiutp_buffer{index = Index2}.

%%------------------------------------------------------------------------------
%% @private
%% @doc 递归初始化空闲链表
%%------------------------------------------------------------------------------
-spec init_free_list(array:array(), non_neg_integer(), non_neg_integer()) ->
    array:array().
init_free_list(Index, Last, Last) ->
    %% 最后一个槽位指向 -1
    array:set(Last, ?END_INDEX, Index);
init_free_list(Index, Cur, Last) ->
    %% 当前槽位指向下一个槽位
    Index2 = array:set(Cur, Cur + 1, Index),
    init_free_list(Index2, Cur + 1, Last).

%%==============================================================================
%% 内部函数 - 槽位分配
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 从空闲链表分配一个槽位
%%
%% @returns {更新后的缓冲区, 分配的索引} 或 {缓冲区, -1} 如果已满
%%------------------------------------------------------------------------------
-spec alloc(aiutp_buffer()) -> {aiutp_buffer(), buffer_index()}.
alloc(#aiutp_buffer{free = ?END_INDEX} = Buffer) ->
    %% 无空闲槽位
    {Buffer, ?END_INDEX};
alloc(#aiutp_buffer{free = Free, index = Index, unused = Unused} = Buffer) ->
    %% 从空闲链表头部取出一个槽位
    NextFree = array:get(Free, Index),
    Buffer2 = Buffer#aiutp_buffer{free = NextFree, unused = Unused - 1},
    {Buffer2, Free}.

%%==============================================================================
%% 内部函数 - 插入操作
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 执行实际的插入操作
%%------------------------------------------------------------------------------
-spec do_insert(buffer_index(), non_neg_integer(), term(), aiutp_buffer()) ->
    aiutp_buffer().
do_insert(?END_INDEX, Pos, Val, Buffer) ->
    %% 插入到链表头部
    insert_at_head(Pos, Val, Buffer);
do_insert(Prev, Pos, Val, Buffer) ->
    %% 插入到 Prev 之后
    insert_after(Prev, Pos, Val, Buffer).

%%------------------------------------------------------------------------------
%% @private
%% @doc 插入到链表头部
%%------------------------------------------------------------------------------
-spec insert_at_head(non_neg_integer(), term(), aiutp_buffer()) -> aiutp_buffer().
insert_at_head(Pos, Val, #aiutp_buffer{data = Data, index = Index,
                                        used = Used, tail = Tail} = Buffer) ->
    %% 新元素指向原来的头部
    Index2 = array:set(Pos, Used, Index),
    Data2 = array:set(Pos, Val, Data),
    %% 如果原来是空的，新元素也是尾部
    Tail2 = case Tail of
        ?END_INDEX -> Pos;
        _ -> Tail
    end,
    Buffer#aiutp_buffer{data = Data2, index = Index2, used = Pos, tail = Tail2}.

%%------------------------------------------------------------------------------
%% @private
%% @doc 在 Prev 之后插入
%%------------------------------------------------------------------------------
-spec insert_after(non_neg_integer(), non_neg_integer(), term(), aiutp_buffer()) ->
    aiutp_buffer().
insert_after(Prev, Pos, Val, #aiutp_buffer{data = Data, index = Index,
                                            tail = Tail} = Buffer) ->
    %% 获取 Prev 的下一个元素
    Next = array:get(Prev, Index),
    %% 新元素指向 Next
    Index2 = array:set(Pos, Next, Index),
    %% Prev 指向新元素
    Index3 = array:set(Prev, Pos, Index2),
    Data2 = array:set(Pos, Val, Data),
    %% 如果 Prev 是尾部，新元素成为尾部
    Tail2 = case Tail of
        Prev -> Pos;
        _ -> Tail
    end,
    Buffer#aiutp_buffer{data = Data2, index = Index3, tail = Tail2}.

%%==============================================================================
%% 内部函数 - 删除操作
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc 删除头部元素
%%------------------------------------------------------------------------------
-spec delete_head(non_neg_integer(), aiutp_buffer()) -> aiutp_buffer().
delete_head(Pos, #aiutp_buffer{data = Data, index = Index, free = Free,
                                unused = Unused, tail = Tail} = Buffer) ->
    %% 清除数据
    Data2 = array:set(Pos, undefined, Data),
    %% 获取原来的下一个元素（成为新头部）
    Next = array:get(Pos, Index),
    %% 将删除的槽位加入空闲链表
    Index2 = array:set(Pos, Free, Index),
    %% 更新尾部（如果删除的是尾部）
    Tail2 = case Tail of
        Pos -> ?END_INDEX;
        _ -> Tail
    end,
    Buffer#aiutp_buffer{
        data = Data2,
        index = Index2,
        used = Next,
        free = Pos,
        unused = Unused + 1,
        tail = Tail2
    }.

%%------------------------------------------------------------------------------
%% @private
%% @doc 删除 Prev 之后的元素
%%------------------------------------------------------------------------------
-spec delete_after(non_neg_integer(), non_neg_integer(), aiutp_buffer()) ->
    aiutp_buffer().
delete_after(Pos, Prev, #aiutp_buffer{data = Data, index = Index, free = Free,
                                       unused = Unused, tail = Tail} = Buffer) ->
    %% 清除数据
    Data2 = array:set(Pos, undefined, Data),
    %% 获取被删除元素的下一个
    Next = array:get(Pos, Index),
    %% Prev 跳过 Pos 指向 Next
    Index2 = array:set(Prev, Next, Index),
    %% 将删除的槽位加入空闲链表
    Index3 = array:set(Pos, Free, Index2),
    %% 更新尾部（如果删除的是尾部）
    Tail2 = case Tail of
        Pos -> Prev;
        _ -> Tail
    end,
    Buffer#aiutp_buffer{
        data = Data2,
        index = Index3,
        free = Pos,
        unused = Unused + 1,
        tail = Tail2
    }.
