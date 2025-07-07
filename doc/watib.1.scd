WATIB(1)

# NAME

watlib - wasm toolchain

# SYNOPSIS

*watib* [options] filename ...

# DESCRIPTION

watib is a user-friendly toolchain for WebAssembly (version 3). It handles
validation, optimisation and assembly of file in the WebAssembly Text format.

For more information on the input format, please refer to the WebAssembly
specification.

# OPTIONS

*-h, --help*
	Display help message and quit.

*-k, --keep-going*
	Continue when encountering an error.

*-v, --validate-only*
	Stop after validation.

*-j* n
	Use n threads.

*-O0*
	Disable optimisations.

*-O2*
	Enable optimisations (default).

*-o* file
	Output binary to file.

# AUTHORS

Mainained by Aghilas Y. Boussaa <aghilas.boussaa@normalesup.org>. For more
information, see <normalesup.org/~boussaa/watib>.
