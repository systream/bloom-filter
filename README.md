Bloom filter
=====

Simple implementation of basic [Bloom filter](https://en.wikipedia.org/wiki/Bloom_filter).  

## Usage

### Create bloom object
It should be set the (block) size of the bloom. 
Let's set it to 1024.  

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