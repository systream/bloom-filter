-module(bloom_filter).

-record(bloom, {bloom_number = 0 :: non_neg_integer(),
                size :: pos_integer(),
                number_of_items = 0 :: non_neg_integer()}).

-type bloom() :: #bloom{}.

-export_type([bloom/0]).

-export([ new/1,
          add/2,
          is_member/2,
          probability/1]).

-spec new(BloomSize :: pos_integer()) -> bloom().
new(BloomSize) ->
  #bloom{size = BloomSize}.

-spec probability(bloom()) -> non_neg_integer().
probability(#bloom{size = Size, number_of_items = ItemCount}) ->
  NumOfHashFuns = 2,
  Result = (1-math:pow((1-(1/ Size)), (NumOfHashFuns * ItemCount))) * 100,
  list_to_integer(erlang:float_to_list(Result, [{decimals, 0}])).

-spec add(Item, Bloom) -> Bloom when
  Item :: term(),
  Bloom :: bloom().
add(Item, #bloom{bloom_number = Bloom, size = Size, number_of_items = ItemCount}) ->
  {
    bloom,
    Bloom bor
      (1 bsl erlang:phash(Item, Size)) bor
      (1 bsl erlang:phash2(Item, Size)),
    Size,
    ItemCount +1
  }.

-spec is_member(Item, Bloom) -> false | maybe when
  Item :: term(),
  Bloom :: bloom().
is_member(Item, #bloom{bloom_number = Bloom, size = Size}) ->
  case Bloom band (1 bsl erlang:phash(Item, Size)) of
    0 ->
      false;
    _ ->
      case Bloom band (1 bsl erlang:phash2(Item, Size)) of
        0 ->
          false;
        _ ->
          maybe
      end
  end.