#!/usr/bin/env python3

import sys


def is_match(rules, message, stack):
    if len(stack) > len(message):
        return False
    elif len(stack) == 0 or len(message) == 0:
        return len(stack) == 0 and len(message) == 0

    entry = stack.pop(0)
    if isinstance(entry, str):
        if message[0] == entry:
            return is_match(rules, message[1:], stack.copy())
    else:
        for rule in rules[entry]:
            if is_match(rules, message, list(rule) + stack):
                return True

    return False


def count_matches(rules, messages, initial_stack):
    count = 0
    for message in messages:
        if is_match(rules, message, initial_stack.copy()):
            count += 1

    return count


if __name__ == '__main__':
    with open(sys.argv[1]) as file:
        raw_rules, messages = [x.splitlines() for x in file.read().split('\n\n')]

    rules = {}
    for rule in raw_rules:
        rule_id, contents = rule.split(': ')
        if contents.startswith('"'):
            rules[int(rule_id)] = contents[1]
        else:
            sub_rules = []
            for entry in contents.split(' | '):
                sub_rules.append([int(sub_rule) for sub_rule in entry.split(' ')])

            rules[int(rule_id)] = sub_rules

    initial_stack = rules[0][0]
    print(f'Part 1: {count_matches(rules, messages, initial_stack)}')

    rules[8] = [[42], [42, 8]]
    rules[11] = [[42, 31], [42, 11, 31]]
    print(f'Part 2: {count_matches(rules, messages, initial_stack)}')
