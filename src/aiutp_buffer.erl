-module(aiutp_buffer).

-export([new/1,
         head/1,
         next/2,
         data/2,
         replace/3,
         delete/3,
         insert/3,
         append/2,
         pop/1,
         size/1]).

-define(LAST_INDEX, -1).
-record(aiutp_buffer,{data,
                      index,
                      used = ?LAST_INDEX,
                      free = 0,
                      size = 0,
                      unused = 0,
                      tail = ?LAST_INDEX}).
new(Size) ->
  Data = array:new(Size),
  Index  = array:new(Size, {default, 0}),
  init(#aiutp_buffer{data = Data, index = Index,
                     size = Size, unused = Size}).

init(#aiutp_buffer{index = Index} = Buffer) ->
  Index2 = init_index(Index, 0, array:size(Index) - 1),
  Buffer#aiutp_buffer{index = Index2}.

init_index(Index, Last, Last) ->
  array:set(Last, ?LAST_INDEX, Index);
init_index(Index, Cur, Last) ->
  Index2 = array:set(Cur, Cur + 1, Index),
  init_index(Index2, Cur + 1, Last).


size(#aiutp_buffer{size = Size}) ->Size.

head(#aiutp_buffer{used = Used}) -> Used.
next(Prev, #aiutp_buffer{index = Index}) -> array:get(Prev, Index).

data(Pos, #aiutp_buffer{data = Data}) -> array:get(Pos, Data).

replace(Pos, Val, #aiutp_buffer{data = Data} = Buffer) ->
  Data2 = array:set(Pos, Val, Data),
  Buffer#aiutp_buffer{data = Data2}.

%% 得到一个可用的索引
alloc(#aiutp_buffer{free = ?LAST_INDEX} = Buffer) -> {Buffer, ?LAST_INDEX};
alloc(#aiutp_buffer{free = Free,index = Index, unused = Unused} = Buffer) ->
  Pos = array:get(Free, Index),%% 下一个可用的位置
  Buffer2 = Buffer#aiutp_buffer{free = Pos, index = Index, unused = Unused - 1},
  {Buffer2, Free}.


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

append(Val, Buffer) -> insert(Buffer#aiutp_buffer.tail, Val, Buffer).
pop(Buffer) -> delete(Buffer#aiutp_buffer.used,?LAST_INDEX,Buffer).
