<<<<<<< HEAD:llvm/lib/CGData/CMakeLists.txt
add_llvm_component_library(LLVMCGData
  CodeGenData.cpp
  CodeGenDataReader.cpp
  CodeGenDataWriter.cpp
=======
add_llvm_component_library(LLVMCodeGenData
>>>>>>> opt:llvm/lib/CodeGenData/CMakeLists.txt
  OutlinedHashTree.cpp
  OutlinedHashTreeRecord.cpp

  ADDITIONAL_HEADER_DIRS
  ${LLVM_MAIN_INCLUDE_DIR}/llvm/CGData

  DEPENDS
  intrinsics_gen

  LINK_COMPONENTS
  Core
  Support
  Object
  )
