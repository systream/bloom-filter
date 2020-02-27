Bloom filter
=====

Simple implementation of basic [Bloom filter](https://en.wikipedia.org/wiki/Bloom_filter).  

## Usage

### Create bloom object
First you should creat a bloom object. For example create an object with max *1024* of bits.  

```erlang
Bloom = bloom_filter:new(1024).
```

### Add item

```erlang
Bloom1 = bloom_filter:add("test", Bloom).
```

### Check item

```erlang
maybe = bloom_filter:is_member("test", Bloom1),
false = bloom_filter:is_member("test2", Bloom1).

``` 

### Probability of false positives
```erlang
0 = bloom_filter:probability(Bloom1),

Bloom2 = lists:foldl(fun(I, BloomAcc) -> 
                      bloom_filter:add(I, BloomAcc) 
                      end, 
                      Bloom, 
                      lists:seq(1, 64)),
                      
12 = bloom_filter:probability(Bloom1).
```

