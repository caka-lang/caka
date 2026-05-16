project('caka', ['c', 'cpp'])

fs = import('fs')

cc = meson.get_compiler('c')
path_include = include_directories('/usr/local/include')

llvm_dep = dependency('llvm', version : '>=18.0', modules : ['core'])

ocamlc = find_program('ocamlc', required: true)
ocaml_where = run_command(ocamlc, '-where', check: true).stdout().strip()
ocaml_dep = declare_dependency(
  compile_args: '-I' + ocaml_where
)

stb_inc = include_directories('stb')

protobuf_dep = dependency('protobuf')
protoc = find_program('protoc')
lib = fs.parent(meson.current_source_dir())
gen = generator(protoc,
  output    : ['@BASENAME@.pb-c.c', '@BASENAME@.pb-c.h'],
  arguments : ['-I'+lib, '--c_out=@BUILD_DIR@', '@INPUT@'])
generated_sources = gen.process(lib/'ast.proto')

static_library(
  'cakabackend',
  'backend.c',
  generated_sources,
  install: true,
  include_directories: stb_inc,
  dependencies: [llvm_dep, ocaml_dep, protobuf_dep],
)

