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

tinhtiendien.function = function(so_kw) {
    muc1 = 1678
    muc2 = 1734
    muc3 = 2014
    muc4 = 2536
    muc5 = 2834
    muc6 = 2927
    bac50 = 50
    bac100 = 100
    
    if (so_kw <= 50) {
        tiendien = so_kw * muc1
    } else if (so_kw <= 100) {
        tiendien = bac50*muc1 + (so_kw-50)*muc2
    } else if (so_kw <= 200) {
        tiendien = bac50*muc1 + bac50*muc2 + (so_kw-bac100)*muc3
    } else if (so_kw <= 300) {
        tiendien = bac50*muc1 + bac50*muc2 + bac100*muc3 + (so_kw-bac50*2-bac100)*muc4
    } else if (so_kw <= 400) {
        tiendien = bac50*muc1 + bac50*muc2 + bac100*muc3 + bac100*muc4 + (so_kw-bac50*2-bac100*2)*muc5
    } else {
        tiendien = bac50*muc1 + bac50*muc2 + bac100*muc3 + bac100*muc4 + bac100*muc5 + (so_kw-bac50*2-bac100*3)*muc6
    }
}

kiem_tra_SNT.function = function(x) {
    i = 1
    y = x/2
    while (i <= y) {
        if (x %% i == 0) {
            res = FALSE
        }
        i = i + 1
    }
    res = TRUE
    return (res)
}

tinh_cuoc_xe.function = function(km, dv) {
    cuoc_gobike = c(10000, 3600)
    cuoc_gosend = c(15000, 4000)
    
    if (dv == 1) {
        gia_dv = cuoc_gobike
    } else {
        gia_dv = cuoc_gosend
    }
    
    if (km <= 2) {
        tien_dv = gia_dv[1]*km
    } else {
        tien_dv = gia_dv[1]*2 + gia_dv[2]*(km-2)
    }
    
    return (tien_dv)
}