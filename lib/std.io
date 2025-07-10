type array(T: type) = struct {
  data: [*]T,
  length: int,

  fun _new(length: int): array(T) do
    return new(struct) array(T) { .data = alloc(length), .length = length };
  end
};

type IntArray = struct {
  data: [*]int,
  length: int,

  fun _new(length: int): IntArray do
    return new(struct) IntArray { .data = alloc(length), .length = length };
  end
};
