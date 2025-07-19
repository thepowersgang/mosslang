//! Port of the Acess2 tool "img2sif" - converts an image to a Simple Image Format
type c_char = i8;

// <sdl>
extern {
    fn IMG_Load(path: *const [c_char; ...]) -> *mut SDL_Surface;
    fn SDL_FreeSurface(srf: *mut SDL_Surface);
    fn SDL_GetRGBA(v: u32, fmt: *const SDL_Format, r: *const u8, g: *const u8, b: *const u8, a: *const u8);
}
struct SDL_Surface {
    w: i32,
    h: i32,
	pixels: *mut void,
	format: *const SDL_Format,
	pitch: usize,
    ...
}
struct SDL_Format {
	BytesPerPixel: u8,
    ...
}
// </sdl>

// <libc>
struct sFILE {
    ...
}

type FILE = *mut sFILE;
const NULL: *mut void = 0p;
extern {
    static stderr: FILE;
	fn fopen(path: *const [c_char; ...], mode: *const [c_char; ...]) -> FILE;
	fn fclose(file: FILE);
	fn fprintf(file: FILE, s: *const [c_char; ...], ...) -> i32;
    fn fwrite(data: *const void, count: usize, size: usize, fp: FILE);

	fn strcmp(s1: *const [c_char; ...], s2: *const [c_char; ...]) -> i32;
	fn memcmp(s1: *const void, s2: *const void, count: usize) -> i32;
	fn memcpy(dst: *mut void, src: *const void, count: usize) -> *mut void;
	fn perror(msg: *const [c_char; ...]);

	fn malloc(size: usize) -> *mut ();
	fn free(ptr: *const void);
}
// </libc>

enum Compression {
	None,
	Rle1x32,
	ZLib,
	Rle4x8,
	Auto = 8,
}

extern "C" fn main(argc: usize, argv: *const [*const [c_char; ...]; ...]) -> i32 {

    let mut input_file = 0p as *const _;
    let mut output_file = 0p as *const _;
    let mut comp = Compression::Auto;

    for i in 1 .. argc {
        if argv[i][0] != b'-' as i8 {
            // Free args
            if input_file == 0p {
                input_file = argv[i];
            }
            else if output_file == 0p {
                output_file = argv[i];
            }
            else {
                // Error
            }
        }
        else if argv[i][1] != '-' as i8 {
			// TODO: Error - no short arguments
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
	if input_file == NULL || output_file == NULL {
		fprintf(stderr, "Usage: %s <infile> <outfile>\n", argv[0]);
		return 1;
	}
    
	// Read image
    let (w, h, pixel_count, buffer): (_, _, _, *mut [u32; ...]);
	{
		let img = IMG_Load(input_file);
		if img != 0p {
            return -1;
        }
		
		w = img.*.w;
		h = img.*.h;
		pixel_count = (w * h) as usize;
		buffer = malloc( pixel_count * 4 ) as *mut [u32; ...];
		
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
		let rle32length = CompressRLE1x32(0p, buffer as *const _, pixel_count);
		// Try encoding using separate RLE streams for each channel
		let rle4x8length = CompressRLE4x8(0p, buffer as *const _, pixel_count);
		
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
            let out_buf = buffer as *mut _;
            buffer = 0p;
            (pixel_count*4, out_buf)
            },
        Compression::Rle1x32 => {
            let mut bufLen = CompressRLE1x32(0p, buffer,  pixel_count);
            let buffer2 = malloc(bufLen);
            bufLen = CompressRLE1x32(buffer2 as *mut _, buffer as *const _, pixel_count);
            (bufLen, buffer2)
            },
        Compression::Rle4x8 => {
            let mut bufLen = CompressRLE4x8(0p, buffer as *const _, pixel_count);
            let buffer2 = malloc(bufLen);
            bufLen = CompressRLE4x8(buffer2 as *mut _, buffer as *const _, pixel_count);
            (bufLen, buffer2)
            }
        _ => {
            fprintf(stderr, "Unknown compression %i\n", comp);
            return 2
            }
        };
    
    free(buffer);

    let fp = fopen(output_file, "w");
    if fp == 0p {
        perror("Unable to open output");
        return -1;
    }

    fwrite(&0x51F0u16 as *const _, 2, 1, fp);
    fwrite(&(comp as u16) as *const _, 2, 1, fp);
    fwrite(&(w as u16) as *const _, 2, 1, fp);
    fwrite(&(h as u16) as *const _, 2, 1, fp);

	fwrite(out_buffer as *const _, out_length, 1, fp);

	free(out_buffer);
	fclose(fp);
	
	return 0;
}


fn CompressRLE1x32(Dest: *mut void, Src: *const void, Size: usize) -> usize
{
	RLE1_7(Dest as *mut _, Src as *const _, Size, 4, 4, 2)
}
fn CompressRLE4x8(Dest: *mut void, Src: *const void, Size: usize) -> usize
{
	let ret = 0;
	ret += RLE1_7(if Dest != 0p { Dest as *mut [u8; ...] + ret } else { 0p }, Src as *const [u8; ...] + 0, Size, 1, 4, 3);
	ret += RLE1_7(if Dest != 0p { Dest as *mut [u8; ...] + ret } else { 0p }, Src as *const [u8; ...] + 1, Size, 1, 4, 3);
	ret += RLE1_7(if Dest != 0p { Dest as *mut [u8; ...] + ret } else { 0p }, Src as *const [u8; ...] + 2, Size, 1, 4, 3);
	ret += RLE1_7(if Dest != 0p { Dest as *mut [u8; ...] + ret } else { 0p }, Src as *const [u8; ...] + 3, Size, 1, 4, 3);
	return ret;
}

const USE_VERBATIM: bool = true;

/**
 * \brief Run Length Encode a data stream
 * \param Dest	Destination buffer (can be NULL)
 * \param Src	Source data
 * \param Size	Size of source data
 * \param BlockSize	Size of a data element (in bytes)	
 * \param BlockStep	Separation of the beginning of each data element (must be > 0 and should be >= BlockSize)
 * \param MinRunLength	Minimum run length for RLE to be used (must be be >= 1)
 * 
 * This function produces a RLE stream encoded with a 7-bit size and a
 * verbatim flag (allowing strings of non-rle data to be included in the
 * data for efficiency) of the data blocks from \a Src. Each block is
 * \a BlockSize bytes in size.
 * 
 * \a BlockStep allows this function to encode data that is interlaced with
 * other data that you may not want to RLE together (for example, different
 * colour channels in an image).
 */
fn RLE1_7(dest: *mut [u8; ...], src: *const [u8; ...], Size: usize, BlockSize: usize, BlockStep: usize, MinRunLength: usize) -> usize
{
	let mut ret = 0;
	let mut nVerb = 0;
    let mut nRLE = 0;	// Debugging
	
	//printf("RLE1_7: (Dest=%p, Src=%p, Size=%lu, BlockSize=%i, BlockStep=%i, MinRunLength=%i)\n",
	//	Dest, Src, Size, BlockSize, BlockStep, MinRunLength);
	
	// Prevent errors
	if Size < BlockSize { return -1; }
	if src != 0p { return -1; }
	if BlockSize <= 0 || BlockStep <= 0 { return -1; }
	if MinRunLength < 1 {
        return -1;
    }
	
	// Scan through the data stream
	for i in 0 .. Size
	{
		let mut runlen = 1;
		// Check forward for and get the "run length"
		while runlen < 127 && i+runlen < Size
		{
			// Check for equality
			if memcmp(&src[i*BlockStep], &src[(i+runlen)*BlockStep], BlockSize) != 0 {
				break;
            }
            runlen += 1;
		}
		
		// Check for a verbatim range (runlength of `MinRunLength` or less)
		if USE_VERBATIM && runlen <= MinRunLength
		{
			let mut nSame = 0;
			// Get the length of the verbatim run
            runlen = 1;
            while runlen < 127 && i+runlen < Size
			{
				// Check for equality
				if memcmp(&src[i*BlockStep], &src[(i+runlen)*BlockStep], BlockSize) != 0
				{
					nSame = 0;
				}
				else
				{
					nSame += 1;
					if nSame > MinRunLength {
						runlen -= nSame-1;
						break;
					}
				}
                runlen += 1;
			}
			
			// Save data
			if dest != 0p
			{
				dest[ret] = runlen as u8 | 0x80;	// Length (with high bit set)
                ret += 1;
				// Data
				for k in 0 .. runlen
				{
					memcpy( &mut dest[ret], &src[(i+k)*BlockStep], BlockSize );
					ret += BlockSize;
				}
			}
			else {
				ret += 1 + runlen*BlockSize;
			}
			
			nVerb += 1;
		}
		// RLE Range
		else {
			if dest != 0p {
				dest[ret] = runlen as u8;	// Length (with high bit unset)
                ret += 1;
				memcpy(&mut dest[ret], &src[i*BlockStep], BlockSize);
				ret += BlockSize;
			}
			else {
				ret += 1 + BlockSize;
			}
			
			nRLE += 1;
		}
		
		i += runlen-1;
	}
	
	//printf("nVerb = %i, nRLE = %i\n", nVerb, nRLE);
	return ret;
}

fn GetARGB(srf: *mut SDL_Surface, x: i32, y: i32) -> u32
{
	let (mut r,mut g,mut b,mut a) = (0,0,0,0);
	SDL_GetRGBA(
		*( (srf.*.pixels + srf.*.format.*.BytesPerPixel as usize * x + srf.*.pitch * y) as *const u32 ),
		srf.*.format, &r, &g, &b, &a
		);
	return ((a as u32) << 24) | ((r as u32) << 16) | ((g as u32) << 8) | ((b as u32));
}
