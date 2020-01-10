# Image Compressor

Description
--
This program is an image compressor made in Haskell with stack. It is used to lower the number of colors in an image using a k-means clustering algorithm.

Dependencies
--
stack

Usage
--
If you are on UNIX you can use the Makefile to build the binary called imageCompressor, otherwise you'll have to install stack and do

```bash
stack build
stack exec imageCompressor n e IN
  n	number of colors in the final image
	e	convergence limit
	IN	path to the file containing the colors of the pixels
```

The input file is not a png or jpg format but a text format specifying the colors of each pixel. To transform an image in this specific text format, I suggest you this tool https://github.com/Az8th/-Epitech-ImageCompressor-Tools

Example
--
Original picture
![landscape](https://user-images.githubusercontent.com/36506539/72159596-fd043100-33bc-11ea-8cc3-e6d3901fe68c.jpg)

Compressed with 4 colors
![landscape4](https://user-images.githubusercontent.com/36506539/72159597-fecdf480-33bc-11ea-8472-5e7afadd8d59.png)

Compressed with 8 colors
![landscape8](https://user-images.githubusercontent.com/36506539/72159601-0097b800-33bd-11ea-8690-5e3b066d8708.png)
