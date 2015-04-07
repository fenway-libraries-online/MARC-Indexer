package MARC::Indexer;

use strict;
use warnings;

use Unicode::Normalize;
use POSIX qw(strftime);

use constant MAXMAX => 2**15;

my $rx_num_range = qr/(?:[0-9]+(?:-[0-9]+)?)/;

sub new {
    my $cls = shift;
    unshift @_, 'index_points' if @_ % 2;
    bless { @_ }, $cls;
}

sub index_points { $_[0]{'index_points'} }

sub set_index_points {
    my ($self, %ip) = @_;
    my $ip = $self->{'index_points'};
    while (my ($name, $v) = each %ip) {
        $ip->{$name} = $v;
    }
    return $self;
}

sub read_index_points {
    my ($self, $f) = @_;
    my $ip = $self->{'index_points'} ||= {};
    open my $fh, '<', $f or die "Can't open $f: $!";
    while (<$fh>) {
        next if /^\s*(?:#.*)?$/;  # Skip blank lines and comments
        die "Bad index point: $_" if !/^\s*([^=\s]+)\s+=\s+(.+)$/;
        $ip->{$1} = $2;
    }
    return $self;
}

sub compile {
    my ($self) = @_;
    if (!$self->{'is_compiled'}) {
        my ($name, %tag, %index_point);
        local $_;
        while (($name, $_) = each %{ $self->{'index_points'} }) {
            # L/06
            # 005 > date2unix
            # 007/00-01 (0,3)
            # 130 < rmnf1
            # 245 < rmnf2 > trim
            # 650[:1=0] > trim, nfd, alpha, upper (1,9)
            # rectype + matdes (*) {--}
            die "Invalid name: $name" if $name !~ /^[a-z]/;
            my %ip = ( 'name' => $name, 'min' => 0, 'max' => MAXMAX, 'type' => 's' );
            while (/\S/) {
                s/\s+$//;
                @ip{qw(min max)}  = minmax($1), next if s/\(([^()+])\)$//;
                $ip{'condition'}  = mkcond($1), next if s/\[\s*([^\[\]]+)\]$//;
                $ip{'munge'}      = mkmung($1), next if s/\<\s*([^<>]+)$//;
                $ip{'select'}     = mksels($1), next if s/\$(\w+)$//;
                $ip{'extract'}    = mkextr($1), next if s/\/\s*($rx_num_range(?:,$rx_num_range)*)$//;
                $ip{'normalize'}  = mknorm($1), next if s/\>\s*([^<>]+)$//;
                $ip{'type'}       = $1 || $2,   next if s/\%(?:(n)um(?:eric)?|(s)tr(?:ing)?)$//;
                $ip{'default'}    = $1,         next if s/\{([^{}]*)\}$//;
                $ip{'parts'}      = mkpart($1), next if s/([a-z]\w+(?:\s*\+\s*[a-z]\w+)+)$//;
                $ip{'ephem'}      = 1,          next if s/\!ephem(?:eral)$//i;
                $tag{$ip{'tag'} = $1||$2} = 1,  next if s/^(L)(?:dr)?|([0-9A-Z]\w\w)//;
                die;
            }
            $index_point{$name} = \%ip;
        }
        $self->{'tags'} = \%tag;
        $self->{'index_points'} = \%index_point;
        $self->{'is_compiled'} = 1;
    }
    return $self;
}

sub mkpart {
    local $_ = shift;
    return [ map { trim($_) } split /\s*\+\s*/ ];
}

sub mkmung {
    local $_ = shift;
    if (/^rmnf([12])$/i) {
        my $in = $1;
        return sub {
            my ($v) = @_;
            return $v if !ref $v;
            my @i = @$v[0..1];
            my @subs = @$v[2..$#$v];
            return $v if !@subs;
            my $i = $i[$in-1];
            return $v if $i == 0;
            substr($subs[0][1], 0, $i) = '';
            return $v;
        };
    }
    else {
        die;
    }
}

sub mksels {
    my %sub = map { $_ => 1 } split //, shift;
    return sub {
        my ($v) = @_;
        die if !ref $v;
        my ($i1, $i2, @subs) = @$v;
        my @subvals;
        foreach (@subs) {
            push @subvals, $_->[1] if $sub{$_->[0]};
        }
        return join(' ', @subvals);
    };
}

sub trim {
    local $_ = shift;
    s/^\s+|\s+$//g;
    return $_;
}

sub mknorm {
    local $_ = shift;
    my @subs = map { __PACKAGE__->can('norm_'.trim($_)) } split /\s*,\s*/;
    return sub {
        my ($v) = @_;
        foreach (@subs) {
            $v = $_->($v);
            return if !defined $v;
        }
        return $v;
    };
}

sub mkextr {
    my ($r) = @_;
    my @ranges;
    foreach (split /,/, $r) {
        my ($b, $e) = split /-/, $_, 2;
        $e = $b if !defined $e;
        my $n = $e - $b + 1;
        push @ranges, [ $b+0, $n ];
    }
    return sub {
        my ($source) = @_;
        my $result = '';
        foreach (@ranges) {
            $result .= substr($source, $_->[0], $_->[1]);
        }
        return $result;
    }
}

sub minmax {
    local $_ = shift;
    return /^\*$/          ? (0, MAXMAX)
         : /^\+$/          ? (1, MAXMAX)
         : /^\?$/          ? (0, 1)
         : /^(\d+),(\d+)$/ ? ($1, $2)
         : /^(\d+),$/      ? ($1, MAXMAX)
         : /^(\d+)$/       ? ($1, $1)
         : die
         ;
}

sub index {
    my ($self, $marc) = @_;
    my $marcref = ref($marc) ? $marc : \$marc;
    # 0. Build the indexing data if needed
    $self->compile if !$self->{'is_compiled'};
    # 1. Collect all contents that will be used in indexing
    my $tags = $self->{'tags'};
    my %field;
    my $baseaddr = substr($$marcref, 12, 5);
    my $directory = substr($$marcref, 24, $baseaddr - 24);
    while ($directory =~ /\G([0-9]{3})([0-9]{4})([0-9]{5})/gc) {
        next if !$tags->{$1};
        my ($tag, $val) = ($1, substr($$marcref, $baseaddr + $3, $2-1));
        if ($tag lt '010') {
            # Control field
            push @{ $field{$tag} ||= [] }, $val;
        }
        else {
            # Data field
            $val =~ /\G(.)(.)/gc or next;
            my @field = ( $1, $2 );
            pos($val) = 2;  # Make sure we've skipped past indicators
            while ($val =~ /\G\x1f(.)([^\x1d-\x1f]+)/gc) {
                my ($sub, $subval) = ($1, $2);
                push @field, [ $sub, $subval ];
            }
            push @{ $field{$tag} ||= [] }, \@field;
        }
    }
    # 2. Normalize and store everything used in each indexing point
    my %index;
    my $ips = $self->{'index_points'};
    # Synthetic index points must be processed after all others
    my @names = sort { ( $ips->{$a}{'parts'} ? 1 : 0 ) <=> ( $ips->{$b}{'parts'} ? 1 : 0 ) } keys %$ips;
    foreach my $name (@names) {
        my $ip = $ips->{$name};
        my ($tag, $parts, $min, $max, $cond, $mung, $extr, $sels, $norm) = @$ip{qw(tag parts min max condition munge extract select normalize)};
        my @vals = $parts      ? $self->synthesize(\%index, @$parts)
                 : $tag eq 'L' ? ( substr($$marcref, 0, 24) )
                 : @{ $field{$tag} ||= [] }
                 ;
        @vals = grep { $cond->($_) } @vals if $cond;
        @vals = map  { $mung->($_) } @vals if $mung;
        @vals = map  { $extr->($_) } @vals if $extr;
        @vals = map  { $sels->($_) } @vals if $sels;
        @vals = map  { $norm->($_) } @vals if $norm;
        @vals = splice @vals, 0, $max if @vals > $max;
        next if @vals < $min;
        $index{$name} = \@vals if @vals;
    }
    return \%index;
}

sub synthesize {
    my ($self, $index) = splice @_, 0, 2;
    my @vals;
    foreach my $p (@_) {
        my $ip = $self->{'index_points'}{$p};
        my ($name, $def, $min, $max) = @$ip{qw(name default min max)};
        my @v = @{ $index->{$name} || ($def ||= []) };
        @v = splice @v, 0, $max if @v > $max;
        @v = @$def if !@v;
        return if @v < $min;
        push @vals, \@v;
    }
    return map { join('', @$_) } permute(@vals);
}

sub permute {
    my $last = pop @_;
    return map [$_], @$last if !@_;
    return map { my $left = $_; map [@$left, $_], @$last } permute(@_);
}

sub mkcond {
    local $_ = shift;
    die "Conditions not yet implemented";
}

sub cond_ind_eq {
    my ($in, $iv) = @_;
    sub {
        my ($val) = @_;
        die "Not a data field value" if !ref $val;
        return $in > 0 && $in <= 2 && $val->[$in-1] eq $iv;
    }
}

sub norm_date2unix {
    local $_ = shift;
    /^([0-9]{4})-?([0-9]{2})-?([0-9]{2})T?([0-9]{2}):?([0-9]{2}):?([0-9]{2})/ or return 0;
    return strftime('%s', $6, $5, $4, $3, $2-1, $1-1900);
}

sub norm_nfc {
    NFC(shift);
}

sub norm_nfd {
    NFD(shift);
}

sub norm_alpha {
    local $_ = shift;
    tr{'}{}d;
    tr{-=./()[],:;"?!}{ };
    s/[^[:alpha] ]//g;
    tr{ }{}s;
    s/^ | $//g;
    return $_
}

sub norm_upper {
    uc shift;
}

sub norm_numeric {
    local $_ = shift;
    die if !/^[0-9]+$/;
    $_ + 0;  # This strips leading zeroes
}

sub norm_trim {
    local $_ = shift;
    tr/ / /s;
    s/^ | $//g;
    return $_;
}

sub norm_isbn {
    local $_ = shift;
    tr/-//d;
    my $n = length;
    die if $n != 10 && $n != 13;
    norm_trim(uc $_);
}



1;

=pod

=head1 NAME

MARC::Indexer - index MARC records

=head1 SYNOPSIS

    $indexer = MARC::Indexer->new(
        'index_points' => [
            { 'name' => 'ctrlnum',
              'tag' => '001' },
            { 'name' => 'title',
              'tag' => '245',
              'extract' => 'subfields:abcdnp',
              'normalize' => [qw(text nfd uc)],
            },
            { 'name' => 'subjtopic',
              'tag' => '650',
              'repeatable' => 1,
              ...
            },
            ...
        ],
    );
    $idx = $indexer->index($marc);
    $idx_title = $idx->{'title'};

=cut

