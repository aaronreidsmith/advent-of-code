from dataclasses import dataclass
from typing import List, Tuple
import sys


@dataclass(unsafe_hash=True)
class Bag:
    adjective: str
    color: str

    def contains_target(self, target, rules):
        rule = rules[self]
        held_bags = [item[0] for item in rule.contents]
        return any(bag == target or bag.contains_target(target, rules) for bag in held_bags)

    def get_contents(self, rules):
        contents = rules[self].contents
        total_bags = 0
        for bag, quantity in contents:
            total_bags += quantity + (quantity * bag.get_contents(rules))

        return total_bags


@dataclass
class Rule:
    bag: Bag
    contents: List[Tuple[Bag, int]]


def parse_line(line):
    bag_desc, contents = line.split(' contain ')

    # Create Bag
    adjective, color, _ = bag_desc.split(' ')
    bag = Bag(adjective, color)

    # Fill in contents
    _contents = []
    if contents != 'no other bags.':
        inner_bags = contents.split(', ')
        for _bag in inner_bags:
            quantity, adjective, color, _ = _bag.split(' ')
            inner_bag = (Bag(adjective, color), int(quantity))
            _contents.append(inner_bag)

    # Turn the contents into a rule and return
    rule = Rule(bag, _contents)
    return (bag, rule)


if __name__ == '__main__':
    input_file = sys.argv[1]

    rules = {}
    with open(input_file) as file:
        for line in file:
            stripped = line.strip()
            bag, rule = parse_line(stripped)
            rules[bag] = rule

    target = Bag('shiny', 'gold')

    # Part 1
    can_hold_target = 0
    for bag in rules.keys():
        if bag.contains_target(target, rules):
            can_hold_target += 1

    # Part 2
    total_contents = target.get_contents(rules)

    print(f'Part 1: {can_hold_target}')
    print(f'Part 2: {total_contents}')
