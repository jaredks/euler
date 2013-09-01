int fib(){
    static int a = 0, b = 1;
    int c = a;
    a = b;
    b += c;
    return c;
}

int main(){
    int n = 0, s = 0;
    while ((n = fib()) < 4000000) if (!(n & 1)) s += n;
    printf("%d\n", s);
}
