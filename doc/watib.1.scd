WATIB(1)

# NAME

watib - wasm toolchain

# SYNOPSIS

*watib* [options] _filename_ ...

# DESCRIPTION

watib is a user-friendly toolchain for WebAssembly (version 3). It handles
validation, optimisation and assembly of file in the WebAssembly Text format.

For more information on the input format, please refer to the WebAssembly
specification.

# OPTIONS

*-h, --help*
	Display help message and quit.

*-o* _file_
	Output binary to file.

*-k, --keep-going*
	Continue when encountering an error.

*-v, --validate-only*
	Stop after validation.

*-j* n
	Use n threads.

*-O0*
	Disable optimisations.

*-O1*
	Enable optimisations (default).

*-fno-testbr*
	Disable type based control flow rewriting.

*-ftestbr*
	Enable type based control flow rewriting.

*-fno-copyprop*
	Disable copy propagation.

*-fcopyprop*
	Enable copy propagation.

*-fno-uncast*
	Disable redundant type tests and casts elimination.

*-funcast*
	Enable redundant type tests and casts elimination.

*-fno-unreachable*
	Disable unreachable code elimination.

*-funreachable*
	Enable unreachable code elimination.

*-fno-const*
	Disable unreachable code elimination.

*-fconst*
	Enable unreachable code elimination.

*-fno-puredrop*
	Disable redundant drop elimination.

*-fpuredrop*
	Enable redundant drop elimination.

# AUTHORS

Mainained by Aghilas Y. Boussaa <aghilas.boussaa@normalesup.org>. For more
information, see <normalesup.org/~boussaa/watib>.
