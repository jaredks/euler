sub fib {
    ($a, $b) = (0, 1);
    return sub {
        ($a, $b) = ($b, $a+$b);
        return $a;
    }
}

my ($f, $n, $s) = (fib(), 0, 0);
while (($n = $f->()) < 4000000) {if (!($n & 1)) {$s += $n}}
print $s, "\n";
