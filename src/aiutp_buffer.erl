-module(aiutp_buffer).

-export([new/1,
         head/1,
         tail/1,
         next/2,
         data/2,
         replace/3,
         delete/3,
         insert/3,
         append/2,
         pop/1,
         size/1,
         used/1,
         unused/1]).

-export_type([aiutp_buffer/0, buffer_index/0]).

-define(LAST_INDEX, -1).

-type buffer_index() :: -1 | non_neg_integer().

-record(aiutp_buffer,{data :: array:array(),
                      index :: array:array(),
                      used = ?LAST_INDEX :: buffer_index(),
                      free = 0 :: buffer_index(),
                      size = 0 :: non_neg_integer(),
                      unused = 0 :: non_neg_integer(),
                      tail = ?LAST_INDEX :: buffer_index()}).

-opaque aiutp_buffer() :: #aiutp_buffer{}.

-spec new(pos_integer()) -> aiutp_buffer().
new(Size) ->
  Data = array:new(Size,{fixed,true}),
  Index  = array:new(Size, [{default, 0},{fixed,true}]),
  init(#aiutp_buffer{data = Data, index = Index,
                     size = Size, unused = Size}).

-spec init(aiutp_buffer()) -> aiutp_buffer().
init(#aiutp_buffer{index = Index} = Buffer) ->
  Index2 = init_index(Index, 0, array:size(Index) - 1),
  Buffer#aiutp_buffer{index = Index2}.

-spec init_index(array:array(), non_neg_integer(), non_neg_integer()) -> array:array().
init_index(Index, Last, Last) ->
  array:set(Last, ?LAST_INDEX, Index);
init_index(Index, Cur, Last) ->
  Index2 = array:set(Cur, Cur + 1, Index),
  init_index(Index2, Cur + 1, Last).

-spec size(aiutp_buffer()) -> non_neg_integer().
size(#aiutp_buffer{size = Size}) ->Size.

-spec used(aiutp_buffer()) -> non_neg_integer().
used(#aiutp_buffer{size = Size, unused = Unused}) -> Size - Unused.

-spec unused(aiutp_buffer()) -> non_neg_integer().
unused(#aiutp_buffer{unused = Unused}) ->  Unused.

-spec head(aiutp_buffer()) -> buffer_index().
head(#aiutp_buffer{used = Used}) -> Used.

-spec next(non_neg_integer(), aiutp_buffer()) -> buffer_index().
next(Prev, #aiutp_buffer{index = Index}) -> array:get(Prev, Index).

-spec tail(aiutp_buffer()) -> buffer_index().
tail(#aiutp_buffer{tail = Tail}) -> Tail.

-spec data(non_neg_integer(), aiutp_buffer()) -> term().
data(Pos, #aiutp_buffer{data = Data}) -> array:get(Pos, Data).

-spec replace(non_neg_integer(), term(), aiutp_buffer()) -> aiutp_buffer().
replace(Pos, Val, #aiutp_buffer{data = Data} = Buffer) ->
  Data2 = array:set(Pos, Val, Data),
  Buffer#aiutp_buffer{data = Data2}.

%% 得到一个可用的索引
-spec alloc(aiutp_buffer()) -> {aiutp_buffer(), buffer_index()}.
alloc(#aiutp_buffer{free = ?LAST_INDEX} = Buffer) -> {Buffer, ?LAST_INDEX};
alloc(#aiutp_buffer{free = Free,index = Index, unused = Unused} = Buffer) ->
  Pos = array:get(Free, Index),%% 下一个可用的位置
  Buffer2 = Buffer#aiutp_buffer{free = Pos, index = Index, unused = Unused - 1},
  {Buffer2, Free}.


-spec delete(non_neg_integer(), buffer_index(), aiutp_buffer()) -> aiutp_buffer().
delete(Pos, ?LAST_INDEX,
       #aiutp_buffer{index = Index, free = Free,data = Data,
                     unused = Unused,tail = Tail} = Buffer) ->
  Data2 = array:set(Pos, undefined, Data),
  Next = array:get(Pos, Index), %% 得到Pos指向的下一个位置
  Index2 = array:set(Pos, Free, Index), %% 将Pos所指向的下一个位置设为next
  Tail2 =
    if Tail =:= Pos -> ?LAST_INDEX; %% 删除队尾
       true -> Tail
    end,
  Buffer#aiutp_buffer{data = Data2, index = Index2,
                      used = Next, free = Pos,
                      unused = Unused + 1, tail = Tail2};

delete(Pos, Prev,
       #aiutp_buffer{index = Index, free = Free,data = Data,
                     unused = Unused,tail = Tail} = Buffer) ->
  Data2 = array:set(Pos, undefined, Data), %% 删除数据
  Next = array:get(Pos, Index),
  Index2 = array:set(Prev, Next, Index), %% 调整链表
  Index3 = array:set(Pos, Free, Index2),
  Tail2 =
    if Tail =:= Pos -> Prev;
       true -> Tail
    end,
  Buffer#aiutp_buffer{data = Data2, index = Index3,
                      free = Pos, unused = Unused + 1, tail = Tail2}.
-spec insert(buffer_index(), term(), aiutp_buffer()) -> aiutp_buffer() | {error, buffer_overflow}.
insert(Prev, Val, Buffer) ->
  case alloc(Buffer) of
    {_, ?LAST_INDEX} -> {error, buffer_overflow};
    {Buffer2, Pos} when Prev =:= ?LAST_INDEX ->
      #aiutp_buffer{data = Data, index = Index,
                    used = Used, tail = Tail} = Buffer2,
      Index2 = array:set(Pos, Used, Index),
      Data2 = array:set(Pos, Val, Data),
      Tail2 =
        if Tail =:= ?LAST_INDEX -> Pos;
           true -> Tail
        end,
      Buffer2#aiutp_buffer{data = Data2, index = Index2,
                           used = Pos, tail = Tail2};
    {Buffer2, Pos} ->
      #aiutp_buffer{data = Data, index = Index, tail = Tail} = Buffer2,
      Next = array:get(Prev, Index),
      Index2 = array:set(Pos, Next, Index),
      Index3 = array:set(Prev, Pos, Index2),
      Data2 = array:set(Pos, Val, Data),
      Tail2 =
        if Tail =:= Prev -> Pos;
           true -> Tail
        end,
      Buffer2#aiutp_buffer{data = Data2, index = Index3, tail = Tail2}
  end.

-spec append(term(), aiutp_buffer()) -> aiutp_buffer() | {error, buffer_overflow}.
append(Val, Buffer) -> insert(Buffer#aiutp_buffer.tail, Val, Buffer).

-spec pop(aiutp_buffer()) -> aiutp_buffer().
pop(Buffer) -> delete(Buffer#aiutp_buffer.used,?LAST_INDEX,Buffer).
