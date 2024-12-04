class FingerTree:
    def __init__(self, root = None, finger = None):
        self.root = root
        self.finger = finger


class Leaf:
    def __init__(self, value):
        self.value = value


class Node:
    def __init__(self, left, right, size):
        self.left = left
        self.right = right
        self.size = size


def search(tree, value):
    if tree is None:
        return None
    if isinstance(tree.root, Leaf):
        return 0 if tree.root.value == value else None
    elif value <= tree.root.left.size:
        index = search(tree.root.left, value)
        return index if index is not None else None
    else:
        index = search(tree.root.right, value - tree.root.left.size)
        return None if index is None else index + tree.root.left.size


def insert(tree, index, value):
    if tree is None:
        return FingerTree(Leaf(value), finger = value)
    if tree.root is None:
        return FingerTree(Leaf(value), finger = value)
    if isinstance(tree.root, Leaf):
        if index == 0:
            return FingerTree(Node(Leaf(value), tree.root, 2), finger = value)
        elif index == 1:
            return FingerTree(Node(tree.root, Leaf(value), 2), finger = value)
        else:
            raise IndexError("Index out of bounds")
    elif index <= tree.root.left.size:
        left = insert(tree.root.left, index, value)
        size = tree.root.size + 1
        if size % 2 == 0:
            return FingerTree(Node(left.root, tree.root.right, size), finger = value)
        else:
            return FingerTree(Node(left.root, tree.root, size), finger = value)
    else:
        right = insert(tree.root.right, index - tree.root.left.size, value)
        size = tree.root.size + 1
        if size % 2 == 0:
            return FingerTree(Node(tree.root.left, right.root, size), finger = value)
        else:
            return FingerTree(Node(tree.root, right.root, size), finger = value)
