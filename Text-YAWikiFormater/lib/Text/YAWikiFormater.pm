package Text::YAWikiFormater;

use 5.006;
use strict;
use warnings;

use HTML::Entities qw(encode_entities);

our $VERSION = '0.01';

my %plugin_tags = (
		toc	=> \&_tag_toc,
	);

my %namespaces = (
		wp	=> { prefix => 'http://en.wikipedia.org/', category=>':' },
	);

my %closed = (
		b		=> qr{(?:(?<!\s)\*\*|\*\*(?!\s))}msix,
		i		=> qr{//},
		u		=> qr{__},
		del	=> qr{(?<!\-)\-\-(?!\-)},
		tt	=> qw{''},

		code	=> [qr[(?<!\{)\{\{\{(?!\{)],qr[(?<!\})\}\}\}(?!\})]],

		blockquote	=> [qr{^[|>]\s}msix, qr{^(?![|>])}msix, qr{^[|>]\s}msix, '',"\n"],
	);

my %nonclosed = (
		hr	=> qr{^[-\*]{3,}\s*?$}msix,
	);

sub new {
	my $class = shift;

	my $self = bless { @_ }, $class;

	return $self;
}

sub urls {
	my $self= shift;
	my $body = $self->{body};

	return unless $body;

	my @links = $body =~m{(\[\[(?:\S[^\|\]]*)(?:\|(?:[^\]]+))?\]\])}g;
	push @links, $body =~m{\s(https?://\S+)\s}g;

	my $links = $self->{_links} ||= {};

	LINK:
	for my $lnk ( @links ) {
		next if $links->{$lnk};

		my $hlnk = $links->{$lnk} ||= {};

		if ($lnk =~ m{\Ahttps?://}) {
			%$hlnk = ( title => $lnk, href => $lnk, _class => 'external' );
			next LINK;
		}
		
		($lnk) = $lnk =~ m{\A\[\[(.*)\]\]\z}g;
		my ($label,$link) = split qr{\|}, $lnk, 2;
		unless ($link) {
			$link = $label;
			if ( $link =~ m{.*[\>\:]([^\>]+)\z} ) {
				$label = $1;
			}
		}

		$hlnk->{title} = $label;
		$hlnk->{original_to} = $link;
		if ($link =~ m{\Ahttps?://} ) {
			$hlnk->{_class} = 'external';
			$hlnk->{href}		= $link;
			next LINK;
		}

		my ($base,$categ) = ('','/');
		if ( $link =~ m{\A(\w+):} ) {
			my ($namespace,$lnk) = split qr{:}, $link, 2;
			$link = $lnk;
			if ( my $nmsp = $namespaces{ $namespace } ){
				if (ref $nmsp eq 'HASH' ) {
					$base 	= $nmsp->{prefix} 	if $nmsp->{prefix};
					$categ 	= $nmsp->{category} if $nmsp->{category};
				} elsif (ref $nmsp eq 'CODE') {
					($base, $categ, $lnk) = $nmsp->($namespace,$link);
					if ( $lnk and $lnk =~ m{\Ahttps?://} ) {
						$hlnk->{href} = $lnk;
						$hlnk->{_class}='external';
						next LINK;
					} elsif ( $lnk ) {
						$link = $lnk;
					}
				}

			} else {
				warn "Unknow namespace: $namespace on $lnk\n";
			}
		}
		
		if ( $categ ) {
			$link =~ s{\>}{$categ}g;
		}
		if ( $base ) {
			$link = $base.$link;
		}
		unless ( $link =~ m{\Ahttps?://} ) {
			$link = urify( $link );
		}
		$hlnk->{href} = $link;
	}

	return wantarray ? %{$self->{_links}} : $self->{_links};
}

sub urify {
	my $link = shift;
	my $reg = "^\\w\\-\\/\\s";
	
	$link = encode_entities( $link, $reg );
	$link =~ s{\s+}{-}g;
	while (my ($ent)=$link=~/\&(\#?\w+);/) {
		my $ec=$ent=~/(acute|grave|circ|uml|ring|slash|tilde|cedil)$/i?
			substr($ent,0,1):'_';
		$link=~s/\&$ent;/$ec/ig;
	}
	$link="\L$link";
	$link=~s/\_+$//g;
	$link=~s/\_+/\_/g;

	return $link;
}

sub set_links {
	
}

sub format {
	my $self = shift;
	my $body = $self->{body};

	# blocks
	for my $tag ( keys %closed ) {
		my ($re1, $re2, $re3, $re4, $re5)
			= ref $closed{ $tag } eq 'ARRAY'
			? @{ $closed{ $tag } }
			: ( $closed{ $tag } );

		if (!$re2) {
			my $in = 0;
			while ( $body =~ m{$re1}msix ) {
				my $tg = $in ? "</$tag>" :"<$tag>";
				$body=~s{$re1}{$tg}msix;
				$in = 1 - $in;
			}
		} else {
			while ($body =~ m{$re1(.*?)$re2}msix) {
				my $in = $1;
				if ($re3) {
					$re4 //= '';
					$in =~ s{ $re3 }{$re4}msixg;
				}
				print STDERR "in: $in\n\n";
				my ($t1,$t2) = ("<$tag>","</$tag>");
				$re5 //= '';
				$body =~ s{$re1(.*?)$re2}{$t1$in$t2$re5}smxi;
			}

		}
	}

	for my $tag ( keys %nonclosed ) {
		my ($re1) = ($nonclosed{ $tag } );

		$body =~ s{ $re1 }{<$tag />}msixg;
	}

	return $body;

}


sub _handle_toc {
	
}

1;
__END__
=head1 NAME

Text::YAWikiFormater - The great new Text::YAWikiFormater!

=head1 VERSION

Version 0.01

=head1 SYNOPSIS

Quick summary of what the module does.

Perhaps a little code snippet.

    use Text::YAWikiFormater;

    my $foo = Text::YAWikiFormater->new();
    ...

=head1 METHODS

Methods WILL be HERE
# TODO:

=head1 AUTHOR

Marco Neves, C<< <perl-cpan at fwd.avidmind.net> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-text-yawikiformater at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Text-YAWikiFormater>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.


=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Text::YAWikiFormater


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Text-YAWikiFormater>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Text-YAWikiFormater>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Text-YAWikiFormater>

=item * Search CPAN

L<http://search.cpan.org/dist/Text-YAWikiFormater/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2013 Marco Neves.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

