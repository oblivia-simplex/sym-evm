# Symbolic Ethereum Virtual Machine (SymEVM)

[Ethereum](https://www.ethereum.org/) is a platform for writing decentralized, trustless applications. 
For a complete, rigorous description of Ethereum, you can visit the [yellow paper](http://gavwood.com/paper.pdf) 
which is also mirrored under the doc/ subdirectory of this repository. Ethereum clients, of which there are 
[m](https://github.com/ethereum/go-ethereum)[a](https://github.com/ethereum/cpp-ethereum)[n](https://github.com/ethereum/pyethereum)[y](https://github.com/ethcore/parity), 
implement the Ethereum Virtual Machine (EVM). They run code hosted on the blockchain (called "contracts"), 
at the request of users on the network, and then publish a block which reflects the state change induced by the code.

This project provides only a symbolic implementation of the Ethereum Virtual Machine. All elements of the environment
are represented purely symbolically, for now. The user can provide EVM bytecode instructions which will be symbolically
executed, and possible errors will be reported with models for the environment and input which would yield those errors.

<!--- TODO: Include a list of bugs that we find -->

This project provides a stripped-down Ethereum client, which implements the EVM symbolically. 
The application is not connected to the Ethereum network. Instead, the user can take a 
[snapshot of the blockchain](https://github.com/ethereum/wiki/wiki/Blockchain-import-and-export-instructions) and seed this client 
using that snapshot. The user then provides a transaction(s) as input. The client will simulate mining those 
transactions into a block, and report any errors it discovers along the way.

## Usage

Once you have it compiled and running, you can use -h or --help to print the usage instructions for the tool.

### Dependencies

  1. [Haskell](https://www.haskell.org/)
  2. [Cabal](https://www.haskell.org/cabal/)
  3. [Stack](https://www.haskellstack.org/)

I develop on a Mac, these are all available as packages via [Homebrew](https://brew.sh/). 
I imagine your system also has packages for these.

### Build

    % stack setup
    % stack build

The `stack setup` stage is only necessary the first time. This will sandbox a GHC install. 
Every time after you need only run `stack build`.

### Run

    % stack exec symevm

### Example

    % ./example.sh
