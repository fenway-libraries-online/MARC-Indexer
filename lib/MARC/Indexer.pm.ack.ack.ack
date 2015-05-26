package MARC::Indexer;

use strict;
use warnings;

use MARC::Indexer::Config;
use Unicode::Normalize;
use POSIX qw(strftime);

sub new {
    my $cls = shift;
    bless { @_ }, $cls;
}

sub compile {
    my ($self) = @_;
    if (!$self->{'is_compiled'}) {
        my %term2eval;
        $self->{'term2eval'} = \%term2eval;
        while (my ($name, $term) = each %{ $self->{'terms'} }) {
            $term2eval{$name} = $self->compile_term_source($name, $term);
        }
    }
    $self->{'is_compiled'} = 1;
}

sub compile_term_source {
    my ($self, $name, $term) = @_;
    my ($source, $norm, $repeat) = @$term{qw(source norm repeat)};
    my @evaluators = @{ $self->{'evaluators'}{$source} ||= [ source2evaluators($name, $source) ] };
    push @evaluators, map { mknorm($_) } @$norm if $norm;
    # ...
    push @evaluators, sub { $_[0] } if !$repeat;
    $term->{'evaluators'} = \@evaluators;
    return sub {
        my @val = @_;
        foreach my $eval (@evaluators) {
            @val = map { $eval->($_) } @val;
            last if !@val;
        }
        return @val;
    };
}

sub source2evaluators {
    my ($name, $source) = @_;
    # L/05
    # 001
    # 245
    # 245!
    # 245$abnpe
    # 336$ba!
    my (%source, @evaluators);
    if ($source =~ m{^(?:L|leader)(/([0-9]+)(?:-([0-9]+))?)?$}) {
        push @evaluators,
            defined $2 ? sub { substr($_, $1, $2-$1+1) }
                       : defined($1) ? sub { substr($_, $1, 1) }
                       : sub { substr($_, 0, 24) };
        $source{'L'}{$name} = \@evaluators;
    }
    else {
        die if $source !~ /^([0-9A-Za-z]{3})/g;
        my $tag = $1;
        $source{$tag}{$name} = \@evaluators;
        if ($tag lt '010') {
            # Control field -- nothing to do unless there's a range of bytes
            if ($source =~ m{\G/([0-9]+)(?:-([0-9]+))?$}) {
                push @evaluators,
                    defined $2 ? sub { substr($_, $1, $2-$1+1) }
                               : sub { substr($_, $1,       1) }
                               ;
            }
        }
        elsif ($source =~ /\G:([12])$/) {
            # First or second indicator
            my $p = $1 - 1;
            push @evaluators, sub { substr($_, $p, 1) };
        }
        elsif ($source =~ /\G\$([0-9A-Za-z]+)(\!?)$/) {
            # Subfields, in the specified order
            my $strip_delims = !length $2;
            my @want = split //, $1;
            my %want = map { $_ => 1 } @want;
            my %have;
            push @evaluators, sub {
                while (m{(\x1f(.)[^\x1d-\x1f]+)}gc) {
                    push @{ $have{$2} ||= [] }, $1 if $want{$2};
                }
                my @subs = map { @{ $have{$_} || [] } } @want;
                return join(' ', map { substr($_, 2) } @subs) if $strip_delims;
                return join('', @subs);
            }
        }
        elsif ($source eq '!') {
            # The whole data field, excluding indicators
            push @evaluators, sub { substr($_, 2) };
        }
        elsif ($source eq '') {
            # All subfields, without delimiters, separated by spaces
            push @evaluators, sub {
                my $val = substr($_, 4);
                $val =~ s/\x1d./ /g;
            };
        }
        else {
            die;
        }
    }
    return @evaluators;
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
            return $v if $i !~ /[1-9]/;
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

sub index {
    my ($self, $marc) = @_;
    my $marcref = ref($marc) ? $marc : \$marc;
    # 0. Compile the indexing code if needed
    $self->compile if !$self->{'is_compiled'};
    # 1. Collect all contents that will be used in indexing
    my $sources = $self->{'sources'};
    my %value;
    if ($sources->{'L'}) {
        $value{'L'} = [ substr($$marcref, 0, 24) ];
    }
    my $baseaddr = substr($$marcref, 12, 5);
    pos($$marcref) = 24;
    while ($$marcref =~ /\G([0-9A-Za-z]{3})([0-9]{4})([0-9]{5})/gc) {
        my $term2eval = $sources->{$1} or next;
        my ($tag, $val) = ($1, $value{$1} = substr($$marcref, $baseaddr + $3, $2-1));
    }
    # 2. Normalize and store everything used in each indexing point
    while (my ($term, $eval) = each %$term2eval) {
        my @val = ($val);
        foreach my $code (@$eval) {
            @val = map { $code->($_) } @val;
            last if !@val;
        }
    }
    push @{ $value{$tag} ||= [] }, $val;
    my %index;
    my $terms = $self->{'terms'};
    # Sort synthetic index points to the end because they must be processed after all others
    my @names = sort { ( $terms->{$a}{'derived'} ? 1 : 0 ) <=> ( $terms->{$b}{'derived'} ? 1 : 0 ) } keys %$terms;
    foreach my $name (@names) {
        my @eval = @{ $terms->{$name} };

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

sub whol_fieldcount {
    local $_ = shift;
    int((CORE::index($$_, "\x1e", 24) - 24) / 12);
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

