type 'a t = Localized of 'a * Nethra_syntax_source.Region.t

let region (Localized (_, r)) = r
