library:
	# Registers
	@bash -c "plant -O <(printf 'let main = (fun x -> (x (fun y -> fun z -> y) (fun y -> fun z -> z)))')"
	@bash -c "plant -O <(printf 'let main = (fun x -> (x (fun y -> fun z -> z) (fun x -> (x (fun z -> fun a -> z) (fun x -> (x (fun a -> fun b -> a) (fun a -> fun b -> b)))))))')"
	@bash -c "plant -O <(printf 'let main = (fun x -> (x (fun y -> fun z -> z) (fun x -> (x (fun z -> fun a -> a) (fun x -> (x (fun a -> fun b -> b) (fun x -> (x (fun b -> fun c -> c) (fun b -> fun c -> c)))))))))')"
	@bash -c "plant -O <(printf 'let main = (fun x -> (x (fun y -> fun z -> z) (fun x -> (x (fun z -> fun a -> a) (fun x -> (x (fun a -> fun b -> a) (fun a -> fun b -> b)))))))')"
	@bash -c "plant -O <(printf 'let main = (fun x -> (x (fun y -> fun z -> z) (fun x -> (x (fun z -> fun a -> z) (fun x -> (x (fun a -> fun b -> b) (fun a -> fun b -> b)))))))')"
	@bash -c "plant -O <(printf 'let main = (fun x -> (x (fun y -> fun z -> z) (fun x -> (x (fun z -> fun a -> a) (fun x -> (x (fun a -> fun b -> b) (fun x -> (x (fun b -> fun c -> b) (fun b -> fun c -> c)))))))))')"

	# enum-cmp
	# CMP-GT
	# CMP-LT
	# CMP-EQ
	# CMP-LE
	# CMP-GE
	# CMP-NE
	@bash -c "plant -O <(printf 'let main = (fun x -> (x (fun y -> fun z -> z) (fun y -> fun z -> z) (fun y -> fun z -> y)))')"
	@bash -c "plant -O <(printf 'let main = (fun x -> (x (fun y -> fun z -> z) (fun y -> fun z -> y) (fun y -> fun z -> z)))')"
	@bash -c "plant -O <(printf 'let main = (fun x -> (x (fun y -> fun z -> y) (fun y -> fun z -> z) (fun y -> fun z -> z)))')"
	@bash -c "plant -O <(printf 'let main = (fun x -> (x (fun y -> fun z -> y) (fun y -> fun z -> y) (fun y -> fun z -> z)))')"
	@bash -c "plant -O <(printf 'let main = (fun x -> (x (fun y -> fun z -> y) (fun y -> fun z -> z) (fun y -> fun z -> y)))')"
	@bash -c "plant -O <(printf 'let main = (fun x -> (x (fun y -> fun z -> z) (fun y -> fun z -> y) (fun y -> fun z -> y)))')"
