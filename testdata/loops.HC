U0 main() {
    I32 i = 70;
    while(i % 100 != 0) {
        if(i == 72) {
            continue;
        } else if(i == 78) {
            break;
        }

        i++;
    }
}
