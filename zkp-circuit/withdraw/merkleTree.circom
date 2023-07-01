pragma circom 2.0.0;

include "../../../circomlib/circuits/sha256/sha256.circom"; // TODO move to node modules on client

// Computes Sha256(left <> right)
template HashLeftRight() {
    signal input left[256];
    signal input right[256];
    signal output hash[256];

    component hasher = Sha256(512);

    for (var i = 0; i < 256; i++) {
        hasher.in[i] <== left[i];
        hasher.in[i + 256] <== right[i];
    }

    for (var i = 0; i < 256; i++) {
        hash[i] <== hasher.out[i];
    }
}

// if s == 0 returns [in[0], in[1]]
// if s == 1 returns [in[1], in[0]]
template DualMux() {
    signal input in[2][256];
    signal input s;
    signal output out[2][256];

    s * (1 - s) === 0;
    for (var i = 0; i < 256; i++) {
        out[0][i] <== (in[1][i] - in[0][i])*s + in[0][i];
        out[1][i] <== (in[0][i] - in[1][i])*s + in[1][i];
    }
}

// Verifies that merkle proof is correct for given merkle root and a leaf
// pathIndices input is an array of 0/1 selectors telling whether given pathElement is on the left or right side of merkle path
template MerkleTreeChecker(levels) {
    signal input leaf[256];
    signal input root[248];
    signal input pathElements[levels][256];
    signal input pathIndices[levels];

    component selectors[levels];
    component hashers[levels];

    for (var l = 0; l < levels; l++) {
        selectors[l] = DualMux();
        selectors[l].s <== pathIndices[l];

        hashers[l] = HashLeftRight();

        for (var i = 0; i < 256; i++) {
            selectors[l].in[0][i] <== l == 0 ? leaf[i] : hashers[l - 1].hash[i];
            selectors[l].in[1][i] <== pathElements[l][i];
        }
        for (var i = 0; i < 256; i++) {
            hashers[l].left[i] <== selectors[l].out[0][i];
            hashers[l].right[i] <== selectors[l].out[1][i];
        }
    }

    for (var i = 0; i < 248; i++) {
        root[i] === hashers[levels - 1].hash[i];
    }
}
