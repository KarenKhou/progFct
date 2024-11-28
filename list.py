class ListNode:
    def __init__(self, value, next_node=None):
        self.value = value
        self.next = next_node

    def __str__(self):
        values = []
        current = self
        while current is not None:
            values.append(str(current.value))
            current = current.next
        return " -> ".join(values)

def map_list(f, lst):
    head = None
    tail = None
    current = lst
    while current:
        new_node = ListNode(f(current.value))
        if head is None:
            head = new_node
        else:
            tail.next = new_node
        tail = new_node
        current = current.next
    return head

def concatenate_lists(lst1, lst2):
    # If lst1 is empty, return lst2
    if lst1 is None:
        return lst2
    # Otherwise, find the tail of lst1 and attach lst2 to it
    current = lst1
    while current.next:
        current = current.next
    current.next = lst2
    return lst1

def insert_at_position(lst, value, position):
    new_node = ListNode(value)
    
    if position == 0:  # If inserting at the head
        new_node.next = lst
        return new_node
    
    current = lst
    for _ in range(position - 1):
        if current is None:
            raise IndexError("Position out of range")
        current = current.next
    
    new_node.next = current.next  # Point the new node to the next of the current node
    current.next = new_node  # Insert the new node after the current node
    
    return lst  # Return the original list head


def filter_list(predicate, lst):
    head = None
    tail = None
    current = lst
    while current:
        if predicate(current.value):
            new_node = ListNode(current.value)
            if head is None:
                head = new_node
            else:
                tail.next = new_node
            tail = new_node
        current = current.next
    return head

def fold_list(f, acc, lst):
    current = lst
    while current:
        acc = f(acc, current.value)
        current = current.next
    return acc

def reverse_list(lst):
    prev = None
    current = lst
    while current:
        next_node = current.next
        current.next = prev
        prev = current
        current = next_node
    return prev

def to_list(py_list):
    if not py_list:
        return None
    head = ListNode(py_list[0])
    current = head
    for value in py_list[1:]:
        current.next = ListNode(value)
        current = current.next
    return head

def from_list(custom_list):
    py_list = []
    while custom_list:
        py_list.append(custom_list.value)
        custom_list = custom_list.next
    return py_list

# Timing and Benchmarking
import time

small_list = to_list([1, 2, 3])
large_list = to_list(range(1, 100001))

# Measure time for map_list on large_list
start = time.time()
_ = map_list(lambda x: x * 2, large_list)
map_time = (time.time() - start) * 1000  # Convert to milliseconds

# Measure time for filter_list on large_list
start = time.time()
_ = filter_list(lambda x: x % 2 == 0, large_list)
filter_time = (time.time() - start) * 1000  # Convert to milliseconds

# Measure time for fold_list on large_list
start = time.time()
_ = fold_list(lambda acc, x: acc + x, 0, large_list)
fold_time = (time.time() - start) * 1000  # Convert to milliseconds

# Measure time for reverse_list on large_list
start = time.time()
_ = reverse_list(large_list)
reverse_time = (time.time() - start) * 1000  # Convert to milliseconds

# Measure time for map_list on small_list
start = time.time()
_ = map_list(lambda x: x * 2, small_list)
small_map_time = (time.time() - start) * 1000  # Convert to milliseconds

# Measure time for filter_list on small_list
start = time.time()
_ = filter_list(lambda x: x % 2 == 0, small_list)
small_filter_time = (time.time() - start) * 1000  # Convert to milliseconds

# Measure time for fold_list on small_list
start = time.time()
_ = fold_list(lambda acc, x: acc + x, 0, small_list)
small_fold_time = (time.time() - start) * 1000  # Convert to milliseconds

# Measure time for reverse_list on small_list
start = time.time()
_ = reverse_list(small_list)
small_reverse_time = (time.time() - start) * 1000  # Convert to milliseconds

{
    "small_map_time": small_map_time,
    "small_filter_time": small_filter_time,
    "small_fold_time": small_fold_time,
    "small_reverse_time": small_reverse_time,
    "map_time": map_time,
    "filter_time": filter_time,
    "fold_time": fold_time,
    "reverse_time": reverse_time
}
