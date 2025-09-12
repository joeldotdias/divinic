U32 thinger(U32 num, U32 j) {
    return 2 * j++ + num;
}

U0 Main() {
    I64 num = thinger(3, 5);
    if(num == 0) {
        num++;
    } else if(num > 2) {
        num *= 2;
    }else {
        num = 84;
    }

    while(num < 5 && num != 10) {
        num--;
    }

    "num = %d", num;
}
