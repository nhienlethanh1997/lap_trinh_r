# Function cua nguoi dung
sum_even = function(start, end) {
    sum_even = 0
    for (i in start:end) {
        if (i%%2 == 0) {
            sum_even = sum_even + i
        }
    }
    return (sum_even)
}

drink_function = function(price, type = "Tea") {
    print(paste("With", price, ", you can drink", type))
}