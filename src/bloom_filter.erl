-module(bloom_filter).

-type bloom() :: {bloom,
                  BloomNumber :: non_neg_integer(),
                  BloomSize :: pos_integer(),
                  NumberOfItems :: non_neg_integer()}.

-export_type([bloom/0]).

-export([new/1,
          add/2,
          is_member/2,
          probability/1]).

-spec new(BloomSize :: pos_integer()) -> bloom().
new(BloomSize) ->
  {bloom, 0, BloomSize, 0}.

-spec probability(bloom()) -> float().
probability({bloom, _, Size, ItemCount}) ->
  NumOfHashFuns = 2,
  Result = math:pow((1-(1/ Size)), (NumOfHashFuns * ItemCount)) * 100,
  list_to_integer(erlang:float_to_list(Result, [{decimals, 0}])).

-spec add(Item, Bloom) -> Bloom when
  Item :: term(),
  Bloom :: bloom().
add(Item, {bloom, Bloom, BloomSize, ItemSize}) ->
  {
    bloom,
    Bloom bor
      (1 bsl erlang:phash(Item, BloomSize)) bor
      (1 bsl erlang:phash2(Item, BloomSize)),
    BloomSize,
    ItemSize+1
  }.

-spec is_member(Item, Bloom) -> false | maybe when
  Item :: term(),
  Bloom :: bloom().
is_member(Item, {bloom, Bloom, BloomSize, _}) ->
  case Bloom band (1 bsl erlang:phash(Item, BloomSize)) of
    0 ->
      false;
    _ ->
      case Bloom band (1 bsl erlang:phash2(Item, BloomSize)) of
        0 ->
          false;
        _ ->
          maybe
      end
  end.