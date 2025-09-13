U0 main() {
    U32 i = 1;
    while (i <= 5) {
        switch (i) {
            case 1:
                "* ";
                break;
            case 2:
                "* * ";
                break;
            case 3:
                "* * * ";
                break;
            case 4:
                "* * * * ";
                break;
            case 5:
                "* * * * * ";
                break;
        }
        "\n";
        i = i + 1;
    }
}
