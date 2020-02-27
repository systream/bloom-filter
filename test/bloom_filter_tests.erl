%%%-------------------------------------------------------------------
%%% @author tihanyipeter
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Feb 2020 21:14
%%%-------------------------------------------------------------------
-module(bloom_filter_tests).
-author("tihanyipeter").

-include_lib("eunit/include/eunit.hrl").

bloom_filter_test() ->
  Bloom = lists:foldl(fun(I, Bloom) ->
                        bloom_filter:add(I, Bloom)
                      end,
                      bloom_filter:new(102400),
                      lists:seq(1, 256)),
  [?assertEqual(maybe, bloom_filter:is_member(I, Bloom)) || I <- lists:seq(1, 256)],
  [?assertEqual(false, bloom_filter:is_member(I, Bloom)) || I <- lists:seq(257, 512)].


probability_test() ->
  Bloom = bloom_filter:new(256),
  ?assertEqual(100, bloom_filter:probability(Bloom)),
  Bloom1 = bloom_filter:add("test1", Bloom),
  ?assertEqual(99, bloom_filter:probability(Bloom1)),
  Bloom2 = lists:foldl(fun(I, BloomAcc) ->
                        bloom_filter:add("test" ++ integer_to_list(I), BloomAcc)
                       end, Bloom1, lists:seq(1, 64)),
  ?assertEqual(60, bloom_filter:probability(Bloom2)).
