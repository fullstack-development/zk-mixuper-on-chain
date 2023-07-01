pragma circom 2.0.0;

include "merkleTree.circom";
include "../../../circomlib/circuits/sha256/sha256.circom"; // TODO move to node modules on client
include "../../../circomlib/circuits/bitify.circom";

template CommitmentHasher() {
    signal input nullifier[248];
    signal input secret[248];
    signal output commitment[256];
    signal output nullifierHash[256];

    component commitmentHasher = Sha256(496);
    component nullifierHasher = Sha256(248);

    for (var i = 0; i < 248; i++) {
        nullifierHasher.in[i] <== nullifier[i];
        commitmentHasher.in[i] <== nullifier[i];
        commitmentHasher.in[i + 248] <== secret[i];
    }

    for (var i = 0; i < 256; i++) {
        commitment[i] <== commitmentHasher.out[i];
        nullifierHash[i] <== nullifierHasher.out[i];
    }
}

// Verifies that commitment that corresponds to given secret and nullifier is included in the merkle tree of deposits
template Withdraw(levels) {
    // *SHA-ARRAY* is sha-256 digest
    // *SHA-ARRAY-TRUNK* is sha-256 digest truncated to 248 bits to be represented as number in the field
    // *gen-ARRAY* is binary string of 248 bits

    // PUBLIC:
    signal input root; // *SHA-ARRAY-TRUNK*
    signal input nullifierHash; // *SHA-ARRAY-TRUNK*
    // blake2b-224 is used for cardano wallet pub key hashes
    // so recipient and relayer are 224-bit and can be interpreted as numbers
    signal input recipient; // not taking part in any computations
    signal input relayer;  // not taking part in any computations
    signal input fee;      // not taking part in any computations

    // PRIVATE:
    signal input nullifier[248]; // *gen-ARRAY*
    signal input secret[248]; // *gen-ARRAY*
    signal input pathElements[levels][256]; // *SHA-ARRAY*
    signal input pathIndices[levels];

    component rootBits = Num2Bits(248);
    component nullifierHashBits = Num2Bits(248);
    rootBits.in <== root;
    nullifierHashBits.in <== nullifierHash;

    component hasher = CommitmentHasher();
    for (var i = 0; i < 248; i++) {
        hasher.nullifier[i] <== nullifier[i];
        hasher.secret[i] <== secret[i];
    }
    for (var i = 0; i < 248; i++) {
        hasher.nullifierHash[i] === nullifierHashBits.out[i];
    }

    component tree = MerkleTreeChecker(levels);
    for (var i = 0; i < 256; i++) {
        tree.leaf[i] <== hasher.commitment[i];
        for (var l = 0; l < levels; l++) {
            tree.pathElements[l][i] <== pathElements[l][i];
        }
    }
    for (var i = 0; i < 248; i++) {
        tree.root[i] <== rootBits.out[i];
    }
    for (var l = 0; l < levels; l++) {
        tree.pathIndices[l] <== pathIndices[l];
    }

    // Add hidden signals to make sure that tampering with recipient or fee will invalidate the snark proof
    // Most likely it is not required, but it's better to stay on the safe side and it only takes 2 constraints
    // Squares are used to prevent optimizer from removing those constraints
    signal recipientSquare;
    signal relayerSquare;
    signal feeSquare;
    recipientSquare <== recipient * recipient;
    relayerSquare <== relayer * relayer;
    feeSquare <== fee * fee;
}

// TODO can we get away with less than 20 levels?
// There are 2^height leaves in a perfect binary tree
// Height = levels
component main {public [root, nullifierHash, recipient, relayer, fee]} = Withdraw(2);
