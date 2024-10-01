def sum_of_divisors(n):
    total = 0
    for i in range(1, n // 2 + 1):
        if n % i == 0:
            total += i
    return total

def find_amicable_numbers(limit):
    amicable_numbers = set()
    
    for number in range(2, limit):
        if number not in amicable_numbers:
            partner = sum_of_divisors(number)
            if partner != number and sum_of_divisors(partner) == number:
                amicable_numbers.add(number)
                amicable_numbers.add(partner)
    
    return sum(amicable_numbers)