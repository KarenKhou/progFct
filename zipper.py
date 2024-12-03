class ListZipper:
    def __init__(self, left, focus, right):
        self.left = left
        self.focus = focus
        self.right = right

    def __str__(self):
        return f"Left: {self.left}, Focus: {self.focus}, Right: {self.right}"

    def move_left(self):
        if not self.left:
            return None  # Cannot move left if already at the beginning
        return ListZipper(self.left[:-1], self.left[-1], [self.focus] + self.right)

    def move_right(self):
        if not self.right:
            return None  # Cannot move right if already at the end
        return ListZipper(self.left + [self.focus], self.right[0], self.right[1:])

    def update_focus(self, value):
        return ListZipper(self.left, value, self.right)

    def to_list(self):
        return self.left + [self.focus] + self.right

# Example usage
lst = [1, 2, 3, 4]
zipper = ListZipper([], lst[0], lst[1:])
print(zipper)  # Left: [], Focus: 1, Right: [2, 3, 4]
zipper = zipper.move_right()
print(zipper)  # Left: [1], Focus: 2, Right: [3, 4]
zipper = zipper.update_focus(5)
print(zipper)  # Left: [1], Focus: 5, Right: [3, 4]
print(zipper.to_list())  # [1, 5, 3, 4]
