package Text::YAWikiFormater;

use 5.006;
use strict;
use warnings;

use HTML::Entities qw(encode_entities);
use JSON qw(from_json);

our $VERSION = '0.02';

my %plugins = (
		toc		=> \&_handle_toc,
		image	=> \&_handle_image,
	);

my %namespaces = (
		wp	=> { prefix => 'http://en.wikipedia.org/', category=>':' },
	);

my %closed = (
		b		=> qr{(?:(?<!\s)\*\*|\*\*(?!\s))}msix,
		i		=> qr{(?<!:)//},
		u		=> qr{__},
		del	=> qr{(?<!\-)\-\-(?!\-)},
		tt	=> qw{''},

		heads	=> [qr[^(?=!{1,6}\s)]msix, qr[$]msix, \&_header_id, undef,"\n"],

		code	=> [qr[^\{\{\{$]msix,qr[^\}\}\}$]msix, \&_escape_code],

		blockquote	=> [qr{^&gt;\s}msix, qr{^(?!&gt;)}msix, qr{^&gt;\s}msix, '',"\n"],

		lists	=> [qr{^(?=[\*\#]+\s)}msix, qr{(?:^(?![\*\#\s])|\z)}msix, \&_do_lists],

		links		=> [qr{(?=\[\[)}, qr{(?<=\]\])},\&_do_links],
		links2	=> [qr{\s(?=http://)}, qr{\s},\&_do_links],

		br		=> [qr{^(?=$)}msix, qr[$]msix, sub { "<br/><br/>",'',''}],

	);

my %nonclosed = (
		hr	=> qr{^[-\*]{3,}\s*?$}msix,
	);

my @do_first = qw( code lists );

sub new {
	my $class = shift;

	my $self = bless { @_ }, $class;

	die "body is a mandatory parameter" unless $self->{body};

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
	my $reg = shift || "^\\w\\-\\/\\s\\#";

	$link =~ s{\s*>\s*}{/}g unless $link =~ m{/};

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
	my ($self, $links) = @_;

	$self->{_links} = $links;

	return;
}

sub format {
	my $self = shift;
	my $body = $self->{body};

	delete $self->{__headers};
	delete $self->{__toc};

	my %done = ();

	$body =~ s{&}{&amp;}g;
	$body =~ s{<}{&lt;}g;
	$body =~ s{>}{&gt;}g;

	# closed tags
	for my $tag ( @do_first, keys %closed ) {
		next if $done{ $tag }++;

		my ($re1, $re2, $re3, $re4, $re5, $re6)
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
				my ($t1,$t2) = ("<$tag>","</$tag>");
				if (ref $re3 eq 'Regexp') {
					$re4 //= '';
					$in =~ s{ $re3 }{$re4}msixg;
				} elsif (ref $re3 eq 'CODE') {
					($t1,$in,$t2) = $re3->($self, $t1, $in, $t2);
				}
				$re5 //= '';
				$body =~ s{$re1(.*?)$re2}{$t1$in$t2$re5}smxi;
			}
		}
	}

	for my $tag ( keys %nonclosed ) {
		my ($re1) = ($nonclosed{ $tag } );

		$body =~ s{ $re1 }{<$tag />}msixg;
	}

	while ($body =~ m[(?<!\{)\{\{(\w+)(?:[:\s]([^\{\}]+))?\}\}(?!\})]msix) {
		my ($plugin, $params) = ($1,$2);
		$params = _parse_plugin_params($params);

		my $res = '';
		if ( $plugins{$plugin} ){
			$res = $plugins{ $plugin }->( $self, $plugin, $params ) // '';
		}

		$body =~ s[(?<!\{)\{\{(\w+)(?:[:\s]([^\{\}]+))?\}\}(?!\})][$res]msix;
	}

	$body =~ s{&plus;}{+}g;
	$body =~ s{&minus;}{-}g;

	return $body;
}

sub register_namespace {
	my $class = shift;

	my ($namespace, $info, $override) = @_;
}

sub _header_id {
	my $self = shift;
	my $headers 	= $self->{__headers} 			||= {};
	my $headnames	= $self->{__headnames} 	  ||= {};
	my $toc 			= $self->{__toc} 					||= [];
	my ($t1, $in, $t2) = @_;

	my ($type) = $in =~ m{^(!{1,6})\s};
	$in =~ s{^!*\s}{};

	$t1 = 'h'.length($type);
	$t2 = "</$t1>";
	$t1 = "<$t1>";

	my $id = urify($in, "^\\w\\-\\s");

	if ($headers->{$id}) {
		my $cnt = 1;
		$cnt++ while $headers->{"${id}_$cnt"};
		$id .= "_$cnt";
	}

	$headnames->{$id} = $in;
	$headers->{$id} 	= substr($t1, 2, 1);
	push @$toc, $id;

	substr($t1, -1, 0, " id='$id'");

	return $t1, $in, $t2;
}

sub _escape_code {
	my $self = shift;

	my ($t1, $in,$t2) = @_;

	$in=~s{\n}{<br/>\n}gs;
	$in=~s{\+}{\&plus;}gs;
	$in=~s{\-}{\&minus;}gs;

	return $t1, $in, $t2;
}

sub _do_lists {
	my $self = shift;

	my ($t1, $in, $t2) = @_;

	my @lines = split qr{\n}ms, $in;
	$in = '';
	my $cl = '';
	my $item;
	for my $ln (@lines) {
		if ( $ln !~ m{^\s} ) {
			if ($item) {
				$in .= "<li>$item</li>\n";
				$item = '';
			}
			my ($nl,$l) = $ln =~ m{^([\*\#]+)\s+(.*)$};
			$ln = $l;
			my $close = '';
			my $start = -1;
			if ($nl ne $cl) {
				for my $i (0..length($cl)-1) {
					next if !$close and substr($cl,$i,1) eq substr($nl, $i, 1);
					$start = $i unless $close;
					$close = (substr($cl,$i,1) eq '#' ? "</ol>" : "</ul>").$close;
				}
				$start = length($cl) if $start == -1;
				$in.=$close."\n" if $close;
				for my $i ($start..length($nl)-1) {
					$in.= substr($nl, $i, 1) eq '#'?"<ol>":"<ul>";
				}
				$cl = $nl;
			}
		}
		$item .= $ln;
	}
	if ($item) {
		$in .= "<li>$item</li>\n";
	}
	if ($cl) {
		for my $i (reverse 0..length($cl)-1) {
			$in.=substr($cl,$i,1) eq '#' ? "</ol>" : "</ul>";
		}
		$in.="\n";
	}

	return '',$in,'';
}

sub _do_links {
	my $self = shift;

	my (undef, $link, undef) = @_;

	$self->urls() unless $self->{_links} and $self->{_links}->{$link};

	my $lnk = $self->{_links}->{$link} || {};

	my ($t1,$t2) = ('','</a>');

	$t1 = "<a href='$lnk->{href}'";
	my $class = $lnk->{class} || $lnk->{_class} || '';
	if ( $class ) {
		$t1.=" class='$class'";
	}
	$t1.='>';

	return $t1, $lnk->{title}, $t2;
}

sub _handle_toc {
	my ($self) = shift;

	my $toc 			= $self->{__toc};
	my $headers 	= $self->{__headers};
	my $headnames	= $self->{__headnames};

	my $res = "\n";
	for my $head (@$toc) {
		$res.='*'x$headers->{$head};
		
		$res.=' ';
		$res.='[['.$headnames->{$head}.'|#'.$head."]]\n";
	}
	$res.="\n";

	my $wf = (ref $self)->new(body => $res);
	$res = $wf->format();

	$res = "<div class='toc'>$res</div>";

	return $res;
}

sub _handle_image {
	my ($self, $plugin, $params) = @_;
	my $src;

	if (ref $params eq 'ARRAY') {
		$src = shift @$params;
		if (@$params and ref $params->[0] eq 'HASH') {
			$params = $params->[0];
		} else {
			$params = { @$params };
		}
	} else {
		$src = delete $params->{src};
	}

	return '<!-- no src - incorrect params? -->' unless $src;

	if ($src =~ m{\Ahttps?://} and $self->{image_filter}) {
		$src = $self->{image_filter}->($src, $params);
	} elsif ($self->{image_mapper}) {
		$src = $self->{image_mapper}->($src, $params);
	}

	return '<!-- image filtered/not mapped -->' unless $src;

	my $res = "<img src='$src'";
	if ( $params->{size} ) {
		my ($w,$h) = $params->{size} =~ m{\A\d+x\d+\z};

		if ($w and $h) {
		 	$params->{width} 	||= $w;
			$params->{height} ||= $h;
		 	delete $params->{size};
		}
	}
	for my $attr ( qw(alt title heigth width) ) {
		next unless $params->{ $attr };
		my $av = $params->{ $attr };
		$av =~ s{&}{&amp;}g;
		$av =~ s{<}{&gt;}g;
		$av	=~ s{>}{&lt;}g;
		$av =~ s{'}{&#39;}g;
		$res.=" $attr='$av'";
	}

	$res.=' />';

	#MAYBETODO: support for caption, to allow to frame the images
	# and add a legend under the image.

	return $res;
}

sub _parse_plugin_params {
	my $paramstr = shift;

	return [] unless $paramstr;

	unless ($paramstr =~ m(\A\s*[\{\[]) ) {
		$paramstr = '['.$paramstr.']';
	}

	my $params = eval {
			from_json( $paramstr, { utf8 => 1 })
		} or do print STDERR "Error Parsing params: $paramstr ==> $@\n";
		#MAYBETODO: export this error somehow? silent it?
		# exporting it may be useful - specially while previewing
		# the result.

	return $params;
}

1;
__END__
=head1 NAME

Text::YAWikiFormater - The great new Text::YAWikiFormater!

=head1 VERSION

Version 0.02

=head1 SYNOPSIS

    use Text::YAWikiFormater;

    my $wiki = Text::YAWikiFormater->new( body => $wikitext );

		my $html = $wiki->format();

=head1 METHODS

=head2 new(body => $wikitext)

Creates the YAWikiFormater object. It accepts the parameters:

=over 4

=item B<body>

body is the wiki text you want to transform into HTML. This parameter
is mandatory.

=back

=head2 $wiki->urls( )

B<urls> extracts all the recognized links from the wikitext and returns
an data structure that you can use to change the parameters that will be
used to generate the final <a> tags on the generated HTML

  my %links = $wiki->urls();
  for my $lnk (value %links) {
    if ($lnk->{_class} eq 'external'
        and $lnk->{href}=~m{wikipedia\.org}) {
      $lnk->{class} = 'external_wikipedia';
      $lnk->{title}.=' (Wikipedia)';
    }
  }
  $wiki->set_links( \%links );

The returned hash contains the link definition text (from your body)
as keys and hashes with the data that will be used to create the <a>
tags as values. On those hashes the following keys are supported:

=over 4

=item B<href>

the url the link will link to (really, the href of the <a> tag).

=item B<class>

the css class that will be used for this link - this is never ser
by C<Text::YAWikiFormater> - it set I<_class> instead, and uses that
if I<class> is not set. _class is set to C<external> for any links
starting with http:// or https://

=item B<title>

I<title> is the content of the link.

=back

=head2 urify($text)

B<urify> is the method used internally to transform wiki titles into wiki
urls - it is export to allow to allow the same algorithm to be used
by any application that uses the TYAWF to generate URLs from text (title
of documents, for instance).

=head2 $wiki->format( )



=head1 WIKI FORMAT

=head1 AUTHOR

Marco Neves, C<< <perl-cpan at fwd.avidmind.net> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-text-yawikiformater at rt.cpan.org>, or through
the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Text-YAWikiFormater>.
I will be notified, and then you'll automatically be notified
of progress on your bug as I make changes.


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

