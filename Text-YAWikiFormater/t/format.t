#!perl -T

use Test::More;

BEGIN {
    use_ok( 'Text::YAWikiFormater' ) || print "Bail out!\n";
}

my $wiki = Text::YAWikiFormater->new(
		body	=> <<EoB,
{{toc}}

**bold**, //italic//, __underline__, --deleted--, ''monospaced''

* list 1
* list 2
* list 3

----

{{{

{ json: [
	{test1: 1 },
	{test2: 2 }
]}

}}}

> Some block test stuff
> With several lines
> And some more lines
> Just to make sure.
Some text in the outside!
EoB
	);

my $res = $wiki->format();

print STDERR $res,"\n";

done_testing();
