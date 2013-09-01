use List::Util qw(sum);
print sum(map {($_ % 3 == 0 || $_ % 5 == 0) ? ($_) : ()} 0..999), "\n";
