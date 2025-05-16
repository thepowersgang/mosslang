//extern libc;
//use libc::{FILE, c_char};

type c_char = i8;
struct FILE { ... }
const SEEK_END: i32 = -1;
extern "C" {
    fn perror(msg: *const c_char);
    fn fprintf(_: *mut FILE, fmt: *const c_char, ...);
    static stderr: *mut FILE;
    fn fopen(path: *const c_char, mode: *const c_char) -> *mut FILE;
    fn fseek(fp: *mut FILE, ofs: i64, dir: i32);
    fn ftell(fp: *mut FILE) -> u64;
    fn fgetc(fp: *mut FILE) -> i32;
    fn fclose(fp: *mut FILE);
    
    // string.h
    fn strerror(errnum: i32) -> *mut c_char;
    fn strlen(s: *const c_char) -> usize;
	fn strcmp(s1: *const c_char, s2: *const c_char) -> i32;
	fn strncmp(s1: *const c_char, s2: *const c_char, max_len: usize) -> i32;
	fn strcpy(d: *mut c_char, s: *const c_char) -> *mut c_char;
	fn strstr(s1: *const c_char, s2: *const c_char) -> *const c_char;
	fn strchr(s1: *const c_char, c: c_char) -> *const c_char;
	fn strrstr(s1: *const c_char, s2: *const c_char) -> *const c_char;
	fn strrchr(s1: *const c_char, c: c_char) -> *const c_char;
	fn memcmp(s1: *const void, s2: *const void, count: usize) -> i32;
	fn memcpy(dst: *mut void, src: *const void, count: usize) -> *mut void;
    
    // stdlib.h
    fn getenv(name: *const c_char) -> *mut c_char;
    fn exit(status: i32);// -> !;
	fn malloc(size: usize) -> *mut void;
	fn realloc(old: *mut void, size: usize) -> *mut void;
	fn free(ptr: *const void);
    static errno: i32;

    // ctype.h
    fn isalnum(ch: i32) -> bool;
    fn isblank(ch: i32) -> bool;
}

enum Token {
    Eof,
    Comment,
    Newline,
    BraceOpen,
    BraceClose,
    Ident,
    String,
}

struct Lexer {
    filename: *const c_char,
    file: *mut FILE,
    cur_line: u32,
    last_token: Token,
    string: *const c_char,
}
struct OutState {
    next_inode_idx: u32,
    
    out_c: *mut FILE,
    out_ldopts: *mut FILE,
    //out_dep: *mut FILE,

    dependencies: *mut *const c_char,
    num_dependencies: usize,
}
struct DirEnt {
    inode: u32,
    name: [c_char; 0],
}
struct DirEnts {
    num_ents: usize,
    ents: *mut *mut DirEnt,
}

extern "C" fn main(argc: usize, argv: *const *const c_char) -> i32 {
    let infile = 0p;
    let outfile_c = 0p;
    let outfile_dep = 0p;
    let outfile_ldopts = 0p;
    
    for arg_i in 1 .. argc
    {
        let arg = argv[arg_i];
        if *arg != '-' {
            if infile == 0p {
                infile = arg;
            }
            else if outfile_c == 0p {
                outfile_c = arg;
            }
            else if outfile_ldopts == 0p {
                outfile_ldopts = arg;
            }
            else if outfile_dep == 0p {
                outfile_dep = arg;
            }
            else {
                fprintf(stderr, "Unexpected free argument\n");
                return 1;
            }
        }
        else {
            fprintf(stderr, "Unexpected argument\n");
            return 1;
        }
    }

    let fp = fopen(infile, "r");
    if fp == 0p {
        perror("Unable to open input file");
        return 1;
    }

    let lexer = Lex_Init(infile, fp);
    let os = OutState {
        next_inode_idx: 1,
		out_c: fopen(outfile_c, "w"),
		out_ldopts: fopen(outfile_ldopts, "w"),
		//out_dep: fopen(outfile_dep, "w"),

        dependencies: 0p,
        num_dependencies: 0,
    };

    fprintf(os.out_c, "\
        /*
        * Acess2 InitRD
        * InitRD Data
        * Generated ???
        */
        #include \"initrd.h\"
        ");
    fprintf(os.out_ldopts, "--format binary\n");


    ProcessDir(&lexer, &os, 0, Token::Eof);


    // --- Finalize the C file (node list)
	fprintf(os.out_c, "tVFS_Node * const gInitRD_FileList[] = {");
	for i in 0 .. os.next_inode_idx {
		fprintf(os.out_c, "&INITRD_%u, ", i);
    }
	fprintf(os.out_c, "};\n");
	
	// --- Close running output files
	fclose(os.out_c);
	fclose(os.out_ldopts);
	
	// --- Write the dependency file
    {
        let depfile = fopen(outfile_dep, "w");
        //assert(depfile);
        fprintf(depfile, "%s:", outfile_c);
        for i in 0 .. os.num_dependencies {
            fprintf(depfile, " %s", os.dependencies[i]);
        }
        fprintf(depfile, "\n");
        for i in 0 .. os.num_dependencies {
            fprintf(depfile, "%s:\n", os.dependencies[i]);
            free(os.dependencies[i]);
        }
        free(os.dependencies);
        fclose(depfile);
    }
}


fn ProcessDir(lex: *mut Lexer, os: *mut OutState, inode: u32, end: Token)
{
	let dents = DirEnts { num_ents: 0, ents: 0p };
	while (lex.*.last_tok != end)
	{
		while (lex.*.last_tok == Token::Newline) {
			Lex_Consume(lex);
        }
		if (lex.*.last_tok == end) {
			break ;
        }
		Lex_Expect(lex, Token::Ident);
		
		if strcmp(lex.*.string, "Dir") == 0 {
			Lex_Consume(lex);
			Lex_Expect(lex, Token::String);
			let name = Lex_ConsumeYield(lex);
			Lex_Expect(lex, Token::BraceOpen);
			Lex_Consume(lex);

			let child_inode = os.*.next_inode_idx;
            os.*.next_inode_idx += 1;
			
			ProcessDir(lex, os, child_inode, Token::BraceClose);
			
			add_dir_ent(&dents, name, child_inode);
			
			free(name);
		}
		else if( strcmp(lex.*.string, "File") == 0 || strcmp(lex.*.string, "OptFile") == 0 ) {
			let is_optional = (lex.*.string[0] == 'O');
			Lex_Consume(lex);
			Lex_Expect(lex, Token::String);
			let path = Lex_ConsumeYield(lex);
			let name = 0p;
			if( lex.*.last_tok == Token::String ) {
				name = path;
				path = Lex_ConsumeYield(lex);
			}
			else {
				let pos = strrchr(path, '/');
				name = strdup(if pos { pos + 1 } else { path });
			}
			Lex_Expect(lex, Token::Newline);
			
			AddFile(os, &dents, is_optional, name, path);
			
			free(name);
			free(path);
		}
		else {
			Lex_Error(lex, "Unexpected entry");
		}
	}

	fprintf(os.*.out_c, "tInitRD_File INITRD_%u_entries[] = {", inode);
	for i in 0 .. dents.num_ents {
		fprintf(os.*.out_c, "{\"%s\",&INITRD_%u}, ", dents.ents[i].*.name, dents.ents[i].*.inode);
		free(dents.ents[i]);
	}
	free(dents.ents);
	fprintf(os.*.out_c, "};\n");
	fprintf(os.*.out_c, "\
        tVFS_Node INITRD_%u = { \
            .NumACLs = 1, \
            .ACLs = &gVFS_ACL_EveryoneRX, \
            .Flags = VFS_FFLAG_DIRECTORY, \
            .Size = %u, \
            .Inode = %u, \
            .ImplPtr = INITRD_%u_entries, \
            .Type = &gInitRD_DirType \
        };\n\n"
		, inode, dents.num_ents, inode, inode
		);
	Lex_Consume(lex);
}
fn add_dir_ent(dents: *mut DirEnts, name: *const c_char, inode: u32) {
	let de: *mut DirEnt = malloc( sizeof(DirEnt) + strlen(name) + 1 );
	de.*.inode = inode;
	strcpy(de.*.name, name);
	
	dents.*.num_ents += 1;
	dents.*.ents = realloc( dents.*.ents, sizeof(*mut DirEnt) * dents.*.num_ents );
	//assert(dents.*.ents);
	dents.*.ents[dents.*.num_ents-1] = de;
}

fn AddFile(os: *mut OutState, dents: *mut DirEnts, is_opt: bool, name: *const c_char, path: *const c_char)
{
	let realpath = {
		let len = expand_path(0p, path);
		let realpath = malloc(len+1);
		expand_path(realpath, path);
        realpath
	};
	
	let file_size = {
		let fp = fopen(realpath, "r");
		if( !fp ) {
			if( is_opt ) {
				free(realpath);
				return ;
			}
			else {
				fprintf(stderr, "Couldn't open '%s': %s\n", realpath, strerror(errno));
				exit(1);
			}
		}
		fseek(fp, 0, SEEK_END);
		let file_size = ftell(fp);
		fclose(fp);
        file_size
	};
	
	let node_idx = os.*.next_inode_idx;
    os.*.next_inode_idx += 1;

	fprintf(os.*.out_ldopts, "%s\n", realpath);
	
	let binary_sym = malloc(strlen(realpath)+1);
	ld_mangle(binary_sym, realpath);
	
	fprintf(os.*.out_c, "extern Uint8 _binary_%s_start[];\n", binary_sym);
	fprintf(os.*.out_c, "\
		tVFS_Node INITRD_%i = {\
			.NumACLs = 1,\
			.ACLs = &gVFS_ACL_EveryoneRX,\
			.Flags = 0,\
			.Size = %u,\
			.Inode = %i,\
			.ImplPtr = _binary_%s_start,\
			.Type = &gInitRD_FileType\
		};\n"
		, node_idx, file_size, node_idx, binary_sym
		);
	free(binary_sym);
	
	os.*.num_dependencies += 1;
	os.*.dependencies = realloc( os.*.dependencies, sizeof(typeof(*os.*.dependencies)) * os.*.num_dependencies );
	//assert(os.*.dependencies);
	os.*.dependencies[os.*.num_dependencies-1] = realpath;
	//free(realpath);

	add_dir_ent(dents, name, node_idx);
}
/// Expand variables in a path
/// 
/// Returns the length of the written string
fn expand_path(out: *mut c_char, src: *mut c_char) -> usize {
	let rv = 0;
	
	while *src != 0 {
		if src[0] == '_' && src[1] == '_' {
			let ARCHDIR = getenv("ARCH");
			if strncmp(src, "__EXT__", 4+3) == 0 {
				src += 4+3;
				
				let s = "../../../../Externals/Output/";
				if out { strcpy(out+rv, s); }
				rv += strlen(s);
				if out { strcpy(out+rv, ARCHDIR); }
				rv += strlen(ARCHDIR);
			}
			else if( strncmp(src, "__BIN__", 4+3) == 0 ) {
				src += 4+3;
				
				let s = "../../../../Usermode/Output/";
				if out { strcpy(out+rv, s); }
				rv += strlen(s);
				if out { strcpy(out+rv, ARCHDIR); }
				rv += strlen(ARCHDIR);
			}
			else if( strncmp(src, "__FS__", 4+2) == 0 ) {
				src += 4+2;
				let s = "../../../../Usermode/Filesystem";
				if out { strcpy(out+rv, s); }
				rv += strlen(s);
			}
			else if( strncmp(src, "__SRC__", 4+3) == 0 ) {
				src += 4+3;
				let s = "../../../..";
				if out { strcpy(out+rv, s); }
				rv += strlen(s);
			}
			else {
				fprintf(stderr, "Encountered unknown replacement at '%s'\n", src);
				exit(1);
			}
		}
		else {
            if out { out[rv] = *src; }
            rv += 1;
			src += 1;
		}
	}
	if out { out[rv] = '\0'; }
	
	return rv;
}
/// Return a version of a name mangled such that it's a valid `ld` symbol
fn ld_mangle(out: *mut c_char, src: *mut c_char) -> usize {
	let rv = 0;
	while *src {
		/*match *src {
		'_' | '$'
        |'0' ... '9'
		|'a' ... 'z'
		|'A' ... 'Z'*/
        if isalnum(*src) || *src == '_' || *src == '$' {
			if out { out[rv] = *src; }
        }
        else {
			if out { out[rv] = '_'; }
        }
        rv += 1;
		src += 1;
	}
	if out { out[rv] = '\0'; }
	return rv;
}

// --------------------------------------------------------------------
// Lexer
// --------------------------------------------------------------------
fn Lex_Init(filename: *const c_char, fp: *mut FILE) -> Lexer
{
    let rv = Lexer {
        filename: filename,
        file: fp,
        string: 0p,
        cur_line: 0,
        last_tok: Token::Newline,
    };
	Lex_Advance(&rv);
	rv
}
fn Lex_Advance(lex: *mut Lexer)
{
    loop {
		//assert(lex.*.string == NULL);
		if( lex.*.last_tok == Token::Newline ) {
			lex.*.cur_line += 1;
		}
		lex.*.last_tok = get_token_int(lex.*.file, &lex.*.string);
        if( lex.*.last_tok != Token::Comment ) {
            return
        }
	}
}
fn Lex_Consume(lex: *mut Lexer)
{
	free(lex.*.string);
	lex.*.string = 0p;
	Lex_Advance(lex);
}
fn Lex_ConsumeYield(lex: *mut Lexer) -> *mut c_char
{
	let rv = lex.*.string;
	lex.*.string = 0p;
	Lex_Advance(lex);
	rv
}
fn Lex_Error(lex: *const Lexer, string: *const c_char)// -> !
{
	fprintf(stderr, "%s:%u: Parse error: %s\n", lex.*.filename, lex.*.cur_line, string);
	exit(1);
}
fn Lex_Expect(lex: *const Lexer, exp: Token)
{
	if( lex.*.last_tok != exp ) {
		fprintf(stderr, "%s:%u: Unexpected token %i \"%s\", expected %i\n", lex.*.filename, lex.*.cur_line, lex.*.last_tok, lex.*.string, exp);
		exit(1);
	}
}

fn get_token_int(ifp: *mut FILE, string: *mut *mut c_char) -> Token {
	let ch = fgetc(ifp);
	
	while isblank(ch) {
		if ch == '\n' {
			return Token::Newline;
        }
		ch = fgetc(ifp);
	}
	
	if ch < 0 {
		return Token::Eof;
	}
	
    /*
	match ch {
	'\n' => Token::Newline,
	'#' => {
		while (ch = fgetc(ifp)) != '\n' {
        }
		ungetc(ch, ifp);
		Token::Comment
        },
	'a' ... 'z'|'A' ... 'Z' => {
		let len = 0;
		let s = 0p;
		while isalnum(ch) {
			len += 1;
			s = realloc(s, len+1);
			assert(s);
			s[len-1] = ch;
			
			ch = fgetc(ifp);
		}
		s[len] = '\0';
		*string = s;
		Token::Ident
        }
	'"' => {
		let len = 0;
		let s = malloc(1);
		while (ch = fgetc(ifp)) != '"' {
			len += 1;
			s = realloc(s, len+1);
			assert(s);
			s[len-1] = ch;
		}
		s[len] = '\0';
		*string = s;
		Token::String
		}
	'{' => Token::BraceOpen,
	'}' => Token::BraceClose,
    _ => {
		fprintf(stderr, "ERROR: Unexpected character '%c'\n", ch);
		exit(1);
        }
	}
    */
}



// --------------------------------------------------------------------
// Extended libc
// --------------------------------------------------------------------
fn strdup(s: *const c_char) -> *mut c_char
{
	let rv = malloc(strlen(s) + 1);
	if rv != 0p {
	    strcpy(rv, s);
    }
	return rv;
}