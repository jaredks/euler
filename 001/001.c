int main(){
    int s = 0, n = 0;
    while (++n < 1000) if (n % 3 == 0 || n % 5 == 0) s += n;
    printf("%d\n", s);
}
