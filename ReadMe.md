# Bege – takes the ‘fun’ out of ‘Befunge’

(The ‘fun’ is the `p` command.)


## Command support

All of Befunge-93 is supported except `p`.

| Command | Supported |
|---------|-----------|
| down `v` | ○ |
| up `^` | ○ |
| right `>` | ○ |
| left `<` | ○ |
| high `h` | × (tre/98) |
| low `l` | × (tre/98) |
| turn left `[` | × (98) |
| turn right `]` | × (98) |
| reflect `r` | ○ (98) |
| push decimal `0`–`9` | ○ |
| push hex `a`–`f` | ○ (98) |
| pop `$` | ○ |
| nop ` ` | ○ |
| skip `#` | ○ |
| exit `@` | ○ |
| duplicate `:` | ○ |
| swap `\\` | ○ |
| clear `n` | ○ (98) |
| input character `~` | ○ |
| output character `,` | ○ |
| input number `&` | ○ |
| output number `.` | ○ |
| not `!` | ○ |
| add `+` | ○ |
| multiply `*` | ○ |
| divide `/` | ○ |
| modulo `%` | ○ |
| subtract `-` | ○ |
| greater <code>`</code> | ○ |
| compare `w` | × (98) |
| branch l/r `_` | ○ |
| branch u/d <code>&#124;</code> | ○ |
| branch h/d `m` | × (tre/98) |
| random `?` | ○ |
| get `g` | ○ |
| put `p` | × |
| store `s` | × (98) |
| input file `i` | × (98) |
| output file `o` | × (98) |
| string mode `"` | ○ |
| comment mode `;` | ○ (98) |
| eval `=` | × (98) | 
| jump `j` | × (98) |
| goto `x` | × (98) |
| iterate `k` | × (98) |
| split `t` | × (98) |
| under `u` | × (98) |
| sysinfo `y` | × (98) |
| begin block `{` | × (98) |
| end block `}` | × (98) |
