//! Port of the Acess2 tool "img2sif" - converts an image to a Simple Image Format
type c_char = i8;

// <sdl>
extern {
    fn IMG_Load(path: *const c_char) -> *mut SDL_Surface;
    fn SDL_FreeSurface(srf: *mut SDL_Surface);
}
struct SDL_Surface {
    w: i32,
    h: i32,
    ...
}
// </sdl>

// <libc>
struct sFILE {
    ...
}
extern {
    static stderr: *mut sFILE;
}
// </libc>

enum Compression {
	None,
	Rle1x32,
	ZLib,
	Rle4x8,
	Auto = 8,
}

extern "C" fn main(argc: usize, argv: *const *const c_char) -> i32 {

    let mut input_file = 0;
    let mut output_file = 0;
    let mut comp = Compression::Auto;

    for i in 1 .. argc {
        if argv[i][0] != b'-' {
            // Free args
            if input_file == 0 {
                input_file = argv[i];
            }
            else if output_file == 0 {
                output_file = argv[i];
            }
            else {
                // Error
            }
        }
        else if argv[i][1] != '-' {
        }
        else {
            if strcmp(argv[i], "--uncompressed") == 0 {
            }
            else if strcmp(argv[i], "--rle4x8") == 0 {
				comp = Compression::Rle4x8;
			}
			else if strcmp(argv[i], "--rle1x32") == 0 {
				comp = Compression::Rle1x32;
			}
			else {
				// Error
			}
		}
	}
	if !input_file || !output_file {
		fprintf(stderr, "Usage: %s <infile> <outfile>\n", argv[0]);
		return 1;
	}
    
	// Read image
    let (w, h, pixel_count, buffer);
	{
		let img = IMG_Load(input_file);
		if !img {
            return -1;
        }
		
		w = img.*.w;
		h = img.*.h;
		pixel_count = w * h;
		buffer = malloc( pixel_count * 4 );
		
        let mut i = 0;
		for y in 0 .. h
		{
			for x in 0 .. w
			{
				buffer[i] = GetARGB(img, x, y);
                i += 1;
			}
		}
		SDL_FreeSurface(img);
	}

	
	if comp == Compression::Auto
	{
		// Try encoding using a single RLE stream
		let rle32length = CompressRLE1x32(0, buffer, pixel_count);
		// Try encoding using separate RLE streams for each channel
		let rle4x8length = CompressRLE4x8(0, buffer, pixel_count);
		
	//	printf("raw length = %i\n", pixel_count * 4);
	//	printf("rle32length = %u\n", (unsigned int)rle32length);
	//	printf("rle4x8length = %u\n", (unsigned int)rle4x8length);
		
		if rle32length <= rle4x8length {
			comp = Compression::Rle1x32;	// 32-bit RLE
		}
		else {
			comp = Compression::Rle4x8;	// 4x8 bit RLE
		}
	}

    let (out_length, out_buffer) = match comp
        {
        Compression::None => {
            let out_buf = buffer;
            buffer = 0;
            (pixel_count*4, buffer)
            },
        Compression::Rle1x32 => {
            let mut bufLen = CompressRLE1x32(NULL, buffer, pixel_count);
            let buffer2 = malloc(bufLen);
            bufLen = CompressRLE1x32(buffer2, buffer, pixel_count);
            (bufLen, buffer2)
            },
        Compression::Rle4x8 => {
            let mut bufLen = CompressRLE4x8(NULL, buffer, pixel_count);
            let buffer2 = malloc(bufLen);
            bufLen = CompressRLE4x8(buffer2, buffer, pixel_count);
            (bufLen, buffer2)
            }
        _ => {
            fprintf(stderr, "Unknown compresion %i\n", comp);
            return 2;
            }
        };
    
    free(buffer);

    let fp = fopen(output_file, "w");
    if !fp {
        perror("Unable to open output");
        return -1;
    }

    fwrite(&0x51F0u16, 2, 1, fp);
    fwrite(&(comp as u16), 2, 1, fp);
    fwrite(&(w as u16), 2, 1, fp);
    fwrite(&(h as u16), 2, 1, fp);

	fwrite(out_buffer, out_length, 1, fp);

	free(out_buffer);
	fclose(fp);
	
	return 0;
}
