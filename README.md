# imagecomp

This program is an image compressor made in Haskell with stack. It is used to lower the number of colors in an image using a k-means clustering algorithm. If you are on UNIX you can use the Makefile to build the binary called imageCompressor, otherwise you'll have to install stack and do

```bash
stack build
stack exec imageCompressor n e IN
  n	number of colors in the final image
	e	convergence limit
	IN	path to the file containing the colors of the pixels
```

Examples:

This picture is the original:
![landscape](https://user-images.githubusercontent.com/36506539/72159596-fd043100-33bc-11ea-8cc3-e6d3901fe68c.jpg)

This one is compressed with 4 colors

./imageCompressor 4 1 path/to/textfile

![landscape4](https://user-images.githubusercontent.com/36506539/72159597-fecdf480-33bc-11ea-8472-5e7afadd8d59.png)


This one is compressed with 8 colors

./imageCompressor 8 1 path/to/textfile
![landscape8](https://user-images.githubusercontent.com/36506539/72159601-0097b800-33bd-11ea-8690-5e3b066d8708.png)
