unit module JamMo;

grammar G {
    token TOP       { <.ws> <nodes> <.ws> }

    rule nodes      { <node>* }
    rule node       { <var> || <partial> || <section> || <not-section> || <text> }

    # treating var and var_no the same for now - do i need escaping?
    token open-tag { '{'**2..3 }
    token close-tag { '}'**2..3 }
    token var  { <open-tag> <.ws> <var-name> <.ws> <close-tag> (<.ws>) }

    token partial    { <open-tag> '>' <.ws> <partial-name> <close-tag> }

    token section-tag { '{{#' }
    token not-section-tag { '{{^' }
    token end-section-tag { '{{/' }

    regex section     { <section-tag> <.ws> (<section-name>) <.ws> <close-tag> $<content>=(.+?) <end-section-tag> <.ws> $0 <.ws> <close-tag> }
    regex not-section { <not-section-tag> <.ws> (<section-name>) <.ws> <close-tag> $<content>=(.+?) <end-section-tag> <.ws> $0 <.ws> <close-tag> }

    token text      { .+? <?before '{{' | $>}

    token var-name  { <[\w\d\_]>+ }
    token partial-name  { <[\w\d\_]>+ }
    token section-name  { <[\w\d\_]>+ }
}

class RenderActions {
    has $.context;
    has $.dir = './templates';
    has $.from;

    method TOP($/) { make $<nodes>.made }
    method nodes($/) { make $<node>>>.made.join }
    method node($/) { make ($<var> || $<text> || $<partial> || $<section> || $<not-section>).made }

    method var($/) { make ($!context{$<var-name>}:exists && $!context{$<var-name>}.so) ?? $!context{$<var-name>} ~ $/[0] !! ''}
    method var_no($/) { make $!context{$<string>}}

    method section($/) {
        my $sname = $/[0];

        if ($!context{$sname}:exists && $!context{$sname}.so) {
            if ($!context{$sname}.WHAT ~~ List) {
                make ($!context{$sname}.map: -> $ctx {
                    my %context = (%$!context, %$ctx);
                    JamMo::render(:template($<content>.Str), :context(%context), :dir($!dir), :from($!from), :inline);
                }).join();
            } else {
                my %context = $!context{$sname}.WHAT ~~ Hash ?? (%$!context, %$!context{$sname}) !! $!context;
                make JamMo::render(:template($<content>.Str), :context(%context), :dir($!dir), :from($!from), :inline);
            }
        } else {
            make '';
        }
    }

    method not-section($/) {
        my $sname = $/[0];

        if ($!context{$sname}:exists && $!context{$sname}.so) {
            make '';
        } else {
            make JamMo::render(:template($<content>.Str), :context($!context), :dir($!dir), :from($!from), :inline)
        }
    }

    method partial($/) { make JamMo::render(:template($<partial-name>), :context($!context), :dir($!dir), :from($!from)) }

    method text($/) { make $/.Str }
}

my $DIR;
my $FROM;

our sub get($name) {
    $FROM{$name} ||= slurp($DIR ~ '/' ~ $name ~ '.html');
    $FROM{$name};
}

our sub render(:$template, :%context, :$dir, :$from, :$inline) {
    $DIR = $dir;
    $FROM = $from;
    G.parse($inline ?? $template !! get($template), :actions(RenderActions.new(:%context, :$dir, :$from))).made;
}
