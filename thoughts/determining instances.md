A type tuple t = (t1, ... tN) can be considered to be an instance of a trait `T` if and only if ∃(τ = (τ1, ... τN)) ∈ *instances*(T) where `all cbu (zip τ t)`.

Awesome way to define specificity: "An instance X is more specific than an instance Y if X could match Y, but not vice versa."

