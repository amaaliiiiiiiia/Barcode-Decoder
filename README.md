# EAN-13 Barcode Decoder
This project implements a decoder for EAN-13 barcodes. The program processes a pixel matrix derived from a black-and-white image, identifies the sequence of bars and spaces, and reconstructs the 13 digits of the code. It also verifies the code's validity using the parity digit and the control digit (checksum).

## Implemented Functions
### 1. Elementary Functions & Bit TDA
- `toBit`: Converts characters or integers into the Bit type.
- `complement`: Inverts a bit (0 to 1 and 1 to 0).
- `group` and `runLength`: Groups consecutive identical elements into separate lists or `(count, element)` pairs to determine bar thickness.
- `leftOddList`, `rightList`, `leftEvenList`: Standard encoding lists `(L, R, G)` generated using higher-order functions.

### 2. Rational Numbers (RatioInt)
- Arithmetic Operations: Implementation of `+`, `-`, `*`, and `/` methods for fractions.
- `compare`: Compares two fractions to determine equality or order.

### 3. Data Transformation and Scaling
- `scaleToOne`: Converts absolute bar lengths into relative frequencies (proportions of the total digit width).
- `scaledRunLength`: Extracts a digit's structure, returning the first bit and a list of relative segment sizes.
- `toParities` and `leftParityList`: Handles L and G parities used to determine the first (parity) digit.

- ### 4. Decoding Algorithm (Core Logic)
- `distance`: Calculates the sum of the absolute differences between the proportions of two bar sets.
- `bestMatch`: Finds the most likely digit from a standard set by minimizing the distance.
- `findLast12Digits`: Splits the 59-bar sequence into individual digits, ignoring start, center, and stop markers.
- `firstDigit`: Determines the parity digit (the first of the 13 digits) based on the L/G encoding combination of the left group.
- `checkDigit`: Calculates the control digit using the weighted EAN-13 formula.
- `verifyCode` and `solve`: Final functions that assemble the 13-digit code and verify its overall integrity.

## Operating Logic
1. Data Input: The program receives data in RLE (Run-Length Encoding) format.
2. Cleaning: Guard sequences (start: 3 bits, center: 5 bits, stop: 3 bits) are identified and ignored.
3. Matching: Each group of 4 bars is compared against the standard L, G encodings (for the left side) and R encodings (for the right side).
4. Selection: The digit with the minimum distance to the ideal proportions is chosen.
5. Parity Detection: The first digit is deduced from the sequence of identified parities in the first group.
6. Checksum: The code is validated using the Check Digit. If valid, the 13-digit string is returned; otherwise, it returns `None`.
