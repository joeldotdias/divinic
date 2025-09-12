U32 thing(U32 num, U32 j) {
    return 2 * 8++ + 5;
}

U0 Main() {
    I64 num = 0;
    if(num == 0) {
        num++;
    } elif(num > 2) {
        num *= 2;
    }else {
        num = 84;
    }

    while(num < 5 && num != 10) {
        num--;
    }

    "num = %d", num;
}
