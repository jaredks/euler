convert = {
    1: "one", 2: "two", 3: "three", 4: "four", 5: "five", 6: "six", 7: "seven", 8: "eight", 9: "nine", 10: "ten",
    11: "eleven", 12: "twelve", 13: "thirteen", 14: "fourteen", 15: "fifteen", 16: "sixteen", 17: "seventeen",
    18: "eighteen", 19: "nineteen", 20: "twenty", 30: "thirty", 40: "forty", 50: "fifty", 60: "sixty", 70: "seventy",
    80: "eighty", 90: "ninety", 100: "hundred", 1000: "thousand"
}

def number_to_word(number):
    word = ""
    place = 1
    if 0 < number % 100 < 20:  # rightmost two digits
        number, rightmost_two = divmod(number, 100)
        word = convert[rightmost_two]
        place = 100
    while number:
        number, digit = divmod(number, 10)
        if digit != 0:
            if place < 100:
                word = convert[digit*place] + word
            else:
                if word:
                    word = "and" + word
                word = convert[digit] + convert[place] + word
        place *= 10
    return word

if __name__ == "__main__":
    #for n in xrange(1, 1001):
    #    print number_to_word(n)
    print sum(len(number_to_word(n)) for n in xrange(1, 1001))
