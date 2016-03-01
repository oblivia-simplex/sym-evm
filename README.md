# Symbolic Ethereum Virtual Machine (SymEVM)

[Ethereum](https://www.ethereum.org/) is a platform for writing decentralized, trustless applications. For a complete, rigorous description of Ethereum,
you can visit the [yellow paper](http://gavwood.com/paper.pdf) which is also mirrored under the doc/ subdirectory of this repository. Ethereum clients,
of which there are [m](https://github.com/ethereum/go-ethereum)[a](https://github.com/ethereum/cpp-ethereum)[n](https://github.com/ethereum/pyethereum)[y](https://github.com/ethcore/parity), implement the Ethereum Virtual Machine (EVM). They run code hosted on the blockchain (called "contracts"), at the request of
users on the network, and then publish a block which reflects the state change induced by the code.

This project implements a stripped-down Ethereum client, which implements the EVM symbolically. The application is not connected to the Ethereum network.
Instead, the user can take a [snapshot of the blockchain](https://github.com/ethereum/wiki/wiki/Blockchain-import-and-export-instructions) and seed this
client using that snapshot. The user then provides a transaction(s) as input. The client will simulate mining those transactions into a block, and report
any errors it discovers along the way.

## Usage

Once you have it compiled and running, you can use -h or --help to print the usage instructions for the tool.

### Dependencies

  1. [Haskell](https://www.haskell.org/)
  2. [Cabal](https://www.haskell.org/cabal/)

I develop on a mac, and these are both available as packages via Homebrew. I would imagine your system also has packages for these.

### Build

__WARNING__: You'll notice that there are these 'cabal sandbox' commands in the Makefile. These ensure that you don't enter dependency
hell induced by cabal. I pity the fool who issues 'cabal install' without a [sandbox](https://www.haskell.org/cabal/users-guide/installing-packages.html#developing-with-sandboxes). If you do so, may [Davy Jones](https://www.youtube.com/watch?v=zo-OYwh6KHA) have mercy on your soul.

    % make

### Run

    % make run

or

    % ./dist/build/symevm/symevm

