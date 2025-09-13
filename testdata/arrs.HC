U0 main() {
    U64 arr[5] = {1,2,3,4,5};
    arr[0] = 7;

    for(U32 i = 0; i < 5; i++) {
        arr[i] = i + 67;
    }
}
