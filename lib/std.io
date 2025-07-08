type array(T: type) = struct {
  data: [*]T,
  length: int,

  fun _new(length: int): array(T) do
    return new array(T) { .data = alloc(length * sizeof(T)), .length = length };
  end
};
