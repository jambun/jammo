unit module JamMo;

grammar G {
    token TOP       { <.ws> <nodes> <.ws> }

    rule nodes      { <node>* }
    rule node       { <var> || <partial> || <section> || <not-section> || <text> }

    # treating var and var_no the same for now - do i need escaping?
    token open-tag { '{'**2..3 }
    token close-tag { '}'**2..3 }
    token var  { <open-tag> <.ws> <var-name> <.ws> <close-tag> (<.ws>) }

    token partial    { <open-tag> '>' <.ws> <partial-name> <.ws> <close-tag> }

    token section-tag { '{{#' }
    token not-section-tag { '{{^' }
    token end-section-tag { '{{/' }

    regex section     { <section-tag> <.ws> (<section-name>) <.ws> <close-tag> $<content>=(.+?) <end-section-tag> <.ws> $0 <.ws> <close-tag> }
    regex not-section { <not-section-tag> <.ws> (<section-name>) <.ws> <close-tag> $<content>=(.+?) <end-section-tag> <.ws> $0 <.ws> <close-tag> }

    token text      { .+? <?before '{{' | $>}

    token var-name  { <[\w\d_]>+ }
    token partial-name  { <[\w\d_.\\]>+ }
    token section-name  { <[\w\d_]>+ }
}

class RenderActions {
    has $.context;
    has $.dir;
    has $.ext;

    method TOP($/) { make $<nodes>.made }
    method nodes($/) { make $<node>>>.made.join }
    method node($/) { make ($<var> || $<text> || $<partial> || $<section> || $<not-section>).made }

    method var($/) { make ($!context{$<var-name>}:exists && $!context{$<var-name>}.so) ?? $!context{$<var-name>} ~ $/[0] !! ''}
    method var_no($/) { make $!context{$<string>}}

    method section($/) {
        my $sname = $/[0].Str;

        if ($!context{$sname}:exists && $!context{$sname}.so) {
            my %context =  $!context;
            if ($!context{$sname}.WHAT ~~ List | Seq) {
                make ($!context{$sname}.map: -> $ctx {
                    %context ,= $ctx.Hash;
                    JamMo::render(:template($<content>.Str), :context(%context), :dir($!dir), :ext($!ext), :inline);
                }).join();
            } else {
                if $!context{$sname} ~~ Hash {
                    %context ,= $!context{$sname}.Hash;
                }
                make JamMo::render(:template($<content>.Str), :context(%context), :dir($!dir), :ext($!ext), :inline);
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
            make JamMo::render(:template($<content>.Str), :context($!context), :dir($!dir), :ext($!ext), :inline)
        }
    }

    method partial($/) { make JamMo::render(:template($<partial-name>.Str), :context($!context), :dir($!dir), :ext($!ext)) }

    method text($/) { make $/.Str }
}

my $default_ext = 'html';
my $template_dir;
my %cache;

our sub default_ext(Str:D $ext?) {
    $default_ext = $ext if $ext;
    $default_ext;
}

our sub template_dir($dir?) {
    if $dir {
        die "$dir is not a directory!" unless $dir.IO.d;
        $template_dir = $dir;
    }
    $template_dir;
}

sub path(Str:D $dir, Str:D $name, Str:D $ext) {
    (($dir, $name).join('/'), $ext).join('.');
}

sub get(Str:D $name, $dir, $ext) {
    die 'No directory provided!' unless $dir || $template_dir;
    my $path = path($dir || $template_dir, $name, $ext || $default_ext);
    die "No template found at: $path" unless $path.IO.f;
    %cache{$path} ||= $path.IO.slurp;
}

our sub render(Str:D :$template! is copy, :%context, :$dir, :$ext is copy, :$inline) {
    # $template is a file name to be found in $dir with $ext (or their defaults) or a template string if $inline
    # in order to support, say, a js template partial inside an html template, $template can include an explicit ext
    if !$inline && $template.comb('.') == 1 {
        ($template, $ext) = $template.split('.');
    }
    G.parse($inline ?? $template !! get($template, $dir, $ext), :actions(RenderActions.new(:%context, :$dir, :$ext))).made;
}
