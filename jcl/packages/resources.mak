ROOTDIR = $(MAKEDIR)\..

# ---------------------------------------------------------------------------
RESFILES = c6\Jcl.res                               \
           c6\JclBaseExpert.res                     \
           c6\JclContainers.res                     \
           c6\JclDebugExpert.res                    \
           c6\JclDebugExpertDLL.res                 \
           c6\JclFavoriteFoldersExpert.res          \
           c6\JclFavoriteFoldersExpertDLL.res       \
           c6\JclProjectAnalysisExpert.res          \
           c6\JclProjectAnalysisExpertDLL.res       \
           c6\JclRepositoryExpert.res               \
           c6\JclRepositoryExpertDLL.res            \
           c6\JclSIMDViewExpert.res                 \
           c6\JclSIMDViewExpertDLL.res              \
           c6\JclDeveloperTools.res                 \
           c6\JclStackTraceViewerExpert.res         \
           c6\JclStackTraceViewerExpertDLL.res      \
           c6\JclThreadNameExpert.res               \
           c6\JclThreadNameExpertDLL.res            \
           c6\JclUsesExpert.res                     \
           c6\JclUsesExpertDLL.res                  \
           c6\JclVcl.res                            \
           c6\JclVersionControlExpert.res           \
           c6\JclVersionControlExpertDLL.res        \
           cs1\Jcl.res                              \
           cs1\JclBaseExpert.res                    \
           cs1\JclContainers.res                    \
           cs1\JclFavoriteFoldersExpertDLL.res      \
           cs1\JclDeveloperTools.res                \
           cs1\JclVersionControlExpertDLL.res       \
           cs1\JclVcl.res                           \
           d10\Jcl.res                              \
           d10\JclBaseExpert.res                    \
           d10\JclContainers.res                    \
           d10\JclDebugExpert.res                   \
           d10\JclDebugExpertDLL.res                \
           d10\JclFavoriteFoldersExpert.res         \
           d10\JclFavoriteFoldersExpertDLL.res      \
           d10\JclProjectAnalysisExpert.res         \
           d10\JclProjectAnalysisExpertDLL.res      \
           d10\JclRepositoryExpert.res              \
           d10\JclRepositoryExpertDLL.res           \
           d10\JclSIMDViewExpert.res                \
           d10\JclSIMDViewExpertDLL.res             \
           d10\JclDeveloperTools.res                \
           d10\JclStackTraceViewerExpert.res        \
           d10\JclStackTraceViewerExpertDLL.res     \
           d10\JclVcl.res                           \
           d10\JclVersionControlExpert.res          \
           d10\JclVersionControlExpertDLL.res       \
           d11\Jcl.res                              \
           d11\JclBaseExpert.res                    \
           d11\JclContainers.res                    \
           d11\JclDebugExpert.res                   \
           d11\JclDebugExpertDLL.res                \
           d11\JclFavoriteFoldersExpert.res         \
           d11\JclFavoriteFoldersExpertDLL.res      \
           d11\JclProjectAnalysisExpert.res         \
           d11\JclProjectAnalysisExpertDLL.res      \
           d11\JclRepositoryExpert.res              \
           d11\JclRepositoryExpertDLL.res           \
           d11\JclSIMDViewExpert.res                \
           d11\JclSIMDViewExpertDLL.res             \
           d11\JclDeveloperTools.res                \
           d11\JclStackTraceViewerExpert.res        \
           d11\JclStackTraceViewerExpertDLL.res     \
           d11\JclVcl.res                           \
           d11\JclVersionControlExpert.res          \
           d11\JclVersionControlExpertDLL.res       \
           d12\Jcl.res                              \
           d12\JclBaseExpert.res                    \
           d12\JclContainers.res                    \
           d12\JclDebugExpert.res                   \
           d12\JclDebugExpertDLL.res                \
           d12\JclFavoriteFoldersExpert.res         \
           d12\JclFavoriteFoldersExpertDLL.res      \
           d12\JclProjectAnalysisExpert.res         \
           d12\JclProjectAnalysisExpertDLL.res      \
           d12\JclRepositoryExpert.res              \
           d12\JclRepositoryExpertDLL.res           \
           d12\JclSIMDViewExpert.res                \
           d12\JclSIMDViewExpertDLL.res             \
           d12\JclDeveloperTools.res                \
           d12\JclStackTraceViewerExpert.res        \
           d12\JclStackTraceViewerExpertDLL.res     \
           d12\JclVcl.res                           \
           d12\JclVersionControlExpert.res          \
           d12\JclVersionControlExpertDLL.res       \
           d14\Jcl.res                              \
           d14\JclBaseExpert.res                    \
           d14\JclContainers.res                    \
           d14\JclDebugExpert.res                   \
           d14\JclDebugExpertDLL.res                \
           d14\JclFavoriteFoldersExpert.res         \
           d14\JclFavoriteFoldersExpertDLL.res      \
           d14\JclProjectAnalysisExpert.res         \
           d14\JclProjectAnalysisExpertDLL.res      \
           d14\JclRepositoryExpert.res              \
           d14\JclRepositoryExpertDLL.res           \
           d14\JclSIMDViewExpert.res                \
           d14\JclSIMDViewExpertDLL.res             \
           d14\JclDeveloperTools.res                \
           d14\JclStackTraceViewerExpert.res        \
           d14\JclStackTraceViewerExpertDLL.res     \
           d14\JclVcl.res                           \
           d14\JclVersionControlExpert.res          \
           d14\JclVersionControlExpertDLL.res       \
           d15\Jcl.res                              \
           d15\JclBaseExpert.res                    \
           d15\JclContainers.res                    \
           d15\JclDebugExpert.res                   \
           d15\JclDebugExpertDLL.res                \
           d15\JclFavoriteFoldersExpert.res         \
           d15\JclFavoriteFoldersExpertDLL.res      \
           d15\JclProjectAnalysisExpert.res         \
           d15\JclProjectAnalysisExpertDLL.res      \
           d15\JclRepositoryExpert.res              \
           d15\JclRepositoryExpertDLL.res           \
           d15\JclSIMDViewExpert.res                \
           d15\JclSIMDViewExpertDLL.res             \
           d15\JclDeveloperTools.res                \
           d15\JclStackTraceViewerExpert.res        \
           d15\JclStackTraceViewerExpertDLL.res     \
           d15\JclVcl.res                           \
           d15\JclVersionControlExpert.res          \
           d15\JclVersionControlExpertDLL.res       \
           d16\Jcl.res                              \
           d16\JclBaseExpert.res                    \
           d16\JclContainers.res                    \
           d16\JclDebugExpert.res                   \
           d16\JclDebugExpertDLL.res                \
           d16\JclFavoriteFoldersExpert.res         \
           d16\JclFavoriteFoldersExpertDLL.res      \
           d16\JclProjectAnalysisExpert.res         \
           d16\JclProjectAnalysisExpertDLL.res      \
           d16\JclRepositoryExpert.res              \
           d16\JclRepositoryExpertDLL.res           \
           d16\JclSIMDViewExpert.res                \
           d16\JclSIMDViewExpertDLL.res             \
           d16\JclDeveloperTools.res                \
           d16\JclStackTraceViewerExpert.res        \
           d16\JclStackTraceViewerExpertDLL.res     \
           d16\JclVcl.res                           \
           d16\JclVersionControlExpert.res          \
           d16\JclVersionControlExpertDLL.res       \
           d17\Jcl.res                              \
           d17\JclBaseExpert.res                    \
           d17\JclContainers.res                    \
           d17\JclDebugExpert.res                   \
           d17\JclDebugExpertDLL.res                \
           d17\JclFavoriteFoldersExpert.res         \
           d17\JclFavoriteFoldersExpertDLL.res      \
           d17\JclProjectAnalysisExpert.res         \
           d17\JclProjectAnalysisExpertDLL.res      \
           d17\JclRepositoryExpert.res              \
           d17\JclRepositoryExpertDLL.res           \
           d17\JclSIMDViewExpert.res                \
           d17\JclSIMDViewExpertDLL.res             \
           d17\JclDeveloperTools.res                \
           d17\JclStackTraceViewerExpert.res        \
           d17\JclStackTraceViewerExpertDLL.res     \
           d17\JclVcl.res                           \
           d17\JclVersionControlExpert.res          \
           d17\JclVersionControlExpertDLL.res       \
           d18\Jcl.res                              \
           d18\JclBaseExpert.res                    \
           d18\JclContainers.res                    \
           d18\JclDebugExpert.res                   \
           d18\JclDebugExpertDLL.res                \
           d18\JclFavoriteFoldersExpert.res         \
           d18\JclFavoriteFoldersExpertDLL.res      \
           d18\JclProjectAnalysisExpert.res         \
           d18\JclProjectAnalysisExpertDLL.res      \
           d18\JclRepositoryExpert.res              \
           d18\JclRepositoryExpertDLL.res           \
           d18\JclSIMDViewExpert.res                \
           d18\JclSIMDViewExpertDLL.res             \
           d18\JclDeveloperTools.res                \
           d18\JclStackTraceViewerExpert.res        \
           d18\JclStackTraceViewerExpertDLL.res     \
           d18\JclVcl.res                           \
           d18\JclVersionControlExpert.res          \
           d18\JclVersionControlExpertDLL.res       \
           d19\Jcl.res                              \
           d19\JclBaseExpert.res                    \
           d19\JclContainers.res                    \
           d19\JclDebugExpert.res                   \
           d19\JclDebugExpertDLL.res                \
           d19\JclFavoriteFoldersExpert.res         \
           d19\JclFavoriteFoldersExpertDLL.res      \
           d19\JclProjectAnalysisExpert.res         \
           d19\JclProjectAnalysisExpertDLL.res      \
           d19\JclRepositoryExpert.res              \
           d19\JclRepositoryExpertDLL.res           \
           d19\JclSIMDViewExpert.res                \
           d19\JclSIMDViewExpertDLL.res             \
           d19\JclDeveloperTools.res                \
           d19\JclStackTraceViewerExpert.res        \
           d19\JclStackTraceViewerExpertDLL.res     \
           d19\JclVcl.res                           \
           d19\JclVersionControlExpert.res          \
           d19\JclVersionControlExpertDLL.res       \
           d20\Jcl.res                              \
           d20\JclBaseExpert.res                    \
           d20\JclContainers.res                    \
           d20\JclDebugExpert.res                   \
           d20\JclDebugExpertDLL.res                \
           d20\JclFavoriteFoldersExpert.res         \
           d20\JclFavoriteFoldersExpertDLL.res      \
           d20\JclProjectAnalysisExpert.res         \
           d20\JclProjectAnalysisExpertDLL.res      \
           d20\JclRepositoryExpert.res              \
           d20\JclRepositoryExpertDLL.res           \
           d20\JclSIMDViewExpert.res                \
           d20\JclSIMDViewExpertDLL.res             \
           d20\JclDeveloperTools.res                \
           d20\JclStackTraceViewerExpert.res        \
           d20\JclStackTraceViewerExpertDLL.res     \
           d20\JclVcl.res                           \
           d20\JclVersionControlExpert.res          \
           d20\JclVersionControlExpertDLL.res       \
           d21\Jcl.res                              \
           d21\JclBaseExpert.res                    \
           d21\JclContainers.res                    \
           d21\JclDebugExpert.res                   \
           d21\JclDebugExpertDLL.res                \
           d21\JclFavoriteFoldersExpert.res         \
           d21\JclFavoriteFoldersExpertDLL.res      \
           d21\JclProjectAnalysisExpert.res         \
           d21\JclProjectAnalysisExpertDLL.res      \
           d21\JclRepositoryExpert.res              \
           d21\JclRepositoryExpertDLL.res           \
           d21\JclSIMDViewExpert.res                \
           d21\JclSIMDViewExpertDLL.res             \
           d21\JclDeveloperTools.res                \
           d21\JclStackTraceViewerExpert.res        \
           d21\JclStackTraceViewerExpertDLL.res     \
           d21\JclVcl.res                           \
           d21\JclVersionControlExpert.res          \
           d21\JclVersionControlExpertDLL.res       \
           d22\Jcl.res                              \
           d22\JclBaseExpert.res                    \
           d22\JclContainers.res                    \
           d22\JclDebugExpert.res                   \
           d22\JclDebugExpertDLL.res                \
           d22\JclFavoriteFoldersExpert.res         \
           d22\JclFavoriteFoldersExpertDLL.res      \
           d22\JclProjectAnalysisExpert.res         \
           d22\JclProjectAnalysisExpertDLL.res      \
           d22\JclRepositoryExpert.res              \
           d22\JclRepositoryExpertDLL.res           \
           d22\JclSIMDViewExpert.res                \
           d22\JclSIMDViewExpertDLL.res             \
           d22\JclDeveloperTools.res                \
           d22\JclStackTraceViewerExpert.res        \
           d22\JclStackTraceViewerExpertDLL.res     \
           d22\JclVcl.res                           \
           d22\JclVersionControlExpert.res          \
           d22\JclVersionControlExpertDLL.res       \
           d23\Jcl.res                              \
           d23\JclBaseExpert.res                    \
           d23\JclContainers.res                    \
           d23\JclDebugExpert.res                   \
           d23\JclDebugExpertDLL.res                \
           d23\JclFavoriteFoldersExpert.res         \
           d23\JclFavoriteFoldersExpertDLL.res      \
           d23\JclProjectAnalysisExpert.res         \
           d23\JclProjectAnalysisExpertDLL.res      \
           d23\JclRepositoryExpert.res              \
           d23\JclRepositoryExpertDLL.res           \
           d23\JclSIMDViewExpert.res                \
           d23\JclSIMDViewExpertDLL.res             \
           d23\JclDeveloperTools.res                \
           d23\JclStackTraceViewerExpert.res        \
           d23\JclStackTraceViewerExpertDLL.res     \
           d23\JclVcl.res                           \
           d23\JclVersionControlExpert.res          \
           d23\JclVersionControlExpertDLL.res       \
           d24\Jcl.res                              \
           d24\JclBaseExpert.res                    \
           d24\JclContainers.res                    \
           d24\JclDebugExpert.res                   \
           d24\JclDebugExpertDLL.res                \
           d24\JclFavoriteFoldersExpert.res         \
           d24\JclFavoriteFoldersExpertDLL.res      \
           d24\JclProjectAnalysisExpert.res         \
           d24\JclProjectAnalysisExpertDLL.res      \
           d24\JclRepositoryExpert.res              \
           d24\JclRepositoryExpertDLL.res           \
           d24\JclSIMDViewExpert.res                \
           d24\JclSIMDViewExpertDLL.res             \
           d24\JclDeveloperTools.res                \
           d24\JclStackTraceViewerExpert.res        \
           d24\JclStackTraceViewerExpertDLL.res     \
           d24\JclVcl.res                           \
           d24\JclVersionControlExpert.res          \
           d24\JclVersionControlExpertDLL.res       \
           d25\Jcl.res                              \
           d25\JclBaseExpert.res                    \
           d25\JclContainers.res                    \
           d25\JclDebugExpert.res                   \
           d25\JclDebugExpertDLL.res                \
           d25\JclFavoriteFoldersExpert.res         \
           d25\JclFavoriteFoldersExpertDLL.res      \
           d25\JclProjectAnalysisExpert.res         \
           d25\JclProjectAnalysisExpertDLL.res      \
           d25\JclRepositoryExpert.res              \
           d25\JclRepositoryExpertDLL.res           \
           d25\JclSIMDViewExpert.res                \
           d25\JclSIMDViewExpertDLL.res             \
           d25\JclDeveloperTools.res                \
           d25\JclStackTraceViewerExpert.res        \
           d25\JclStackTraceViewerExpertDLL.res     \
           d25\JclVcl.res                           \
           d25\JclVersionControlExpert.res          \
           d25\JclVersionControlExpertDLL.res       \
           d26\Jcl.res                              \
           d26\JclBaseExpert.res                    \
           d26\JclContainers.res                    \
           d26\JclDebugExpert.res                   \
           d26\JclDebugExpertDLL.res                \
           d26\JclFavoriteFoldersExpert.res         \
           d26\JclFavoriteFoldersExpertDLL.res      \
           d26\JclProjectAnalysisExpert.res         \
           d26\JclProjectAnalysisExpertDLL.res      \
           d26\JclRepositoryExpert.res              \
           d26\JclRepositoryExpertDLL.res           \
           d26\JclSIMDViewExpert.res                \
           d26\JclSIMDViewExpertDLL.res             \
           d26\JclDeveloperTools.res                \
           d26\JclStackTraceViewerExpert.res        \
           d26\JclStackTraceViewerExpertDLL.res     \
           d26\JclVcl.res                           \
           d26\JclVersionControlExpert.res          \
           d26\JclVersionControlExpertDLL.res       \
           d27\Jcl.res                              \
           d27\JclBaseExpert.res                    \
           d27\JclContainers.res                    \
           d27\JclDebugExpert.res                   \
           d27\JclDebugExpertDLL.res                \
           d27\JclFavoriteFoldersExpert.res         \
           d27\JclFavoriteFoldersExpertDLL.res      \
           d27\JclProjectAnalysisExpert.res         \
           d27\JclProjectAnalysisExpertDLL.res      \
           d27\JclRepositoryExpert.res              \
           d27\JclRepositoryExpertDLL.res           \
           d27\JclSIMDViewExpert.res                \
           d27\JclSIMDViewExpertDLL.res             \
           d27\JclDeveloperTools.res                \
           d27\JclStackTraceViewerExpert.res        \
           d27\JclStackTraceViewerExpertDLL.res     \
           d27\JclVcl.res                           \
           d27\JclVersionControlExpert.res          \
           d27\JclVersionControlExpertDLL.res       \
           d28\Jcl.res                              \
           d28\JclBaseExpert.res                    \
           d28\JclContainers.res                    \
           d28\JclDebugExpert.res                   \
           d28\JclDebugExpertDLL.res                \
           d28\JclFavoriteFoldersExpert.res         \
           d28\JclFavoriteFoldersExpertDLL.res      \
           d28\JclProjectAnalysisExpert.res         \
           d28\JclProjectAnalysisExpertDLL.res      \
           d28\JclRepositoryExpert.res              \
           d28\JclRepositoryExpertDLL.res           \
           d28\JclSIMDViewExpert.res                \
           d28\JclSIMDViewExpertDLL.res             \
           d28\JclDeveloperTools.res                \
           d28\JclStackTraceViewerExpert.res        \
           d28\JclStackTraceViewerExpertDLL.res     \
           d28\JclVcl.res                           \
           d28\JclVersionControlExpert.res          \
           d28\JclVersionControlExpertDLL.res       \
           d29\Jcl.res                              \
           d29\JclBaseExpert.res                    \
           d29\JclContainers.res                    \
           d29\JclDebugExpert.res                   \
           d29\JclDebugExpertDLL.res                \
           d29\JclFavoriteFoldersExpert.res         \
           d29\JclFavoriteFoldersExpertDLL.res      \
           d29\JclProjectAnalysisExpert.res         \
           d29\JclProjectAnalysisExpertDLL.res      \
           d29\JclRepositoryExpert.res              \
           d29\JclRepositoryExpertDLL.res           \
           d29\JclSIMDViewExpert.res                \
           d29\JclSIMDViewExpertDLL.res             \
           d29\JclDeveloperTools.res                \
           d29\JclStackTraceViewerExpert.res        \
           d29\JclStackTraceViewerExpertDLL.res     \
           d29\JclVcl.res                           \
           d29\JclVersionControlExpert.res          \
           d29\JclVersionControlExpertDLL.res       \
           d6\Jcl.res                               \
           d6\JclBaseExpert.res                     \
           d6\JclContainers.res                     \
           d6\JclDebugExpert.res                    \
           d6\JclDebugExpertDLL.res                 \
           d6\JclFavoriteFoldersExpert.res          \
           d6\JclFavoriteFoldersExpertDLL.res       \
           d6\JclProjectAnalysisExpert.res          \
           d6\JclProjectAnalysisExpertDLL.res       \
           d6\JclRepositoryExpert.res               \
           d6\JclRepositoryExpertDLL.res            \
           d6\JclSIMDViewExpert.res                 \
           d6\JclSIMDViewExpertDLL.res              \
           d6\JclDeveloperTools.res                 \
           d6\JclStackTraceViewerExpert.res         \
           d6\JclStackTraceViewerExpertDLL.res      \
           d6\JclThreadNameExpert.res               \
           d6\JclThreadNameExpertDLL.res            \
           d6\JclUsesExpert.res                     \
           d6\JclUsesExpertDLL.res                  \
           d6\JclVcl.res                            \
           d6\JclVersionControlExpert.res           \
           d6\JclVersionControlExpertDLL.res        \
           d7\Jcl.res                               \
           d7\JclBaseExpert.res                     \
           d7\JclContainers.res                     \
           d7\JclDebugExpert.res                    \
           d7\JclDebugExpertDLL.res                 \
           d7\JclFavoriteFoldersExpert.res          \
           d7\JclFavoriteFoldersExpertDLL.res       \
           d7\JclProjectAnalysisExpert.res          \
           d7\JclProjectAnalysisExpertDLL.res       \
           d7\JclRepositoryExpert.res               \
           d7\JclRepositoryExpertDLL.res            \
           d7\JclSIMDViewExpert.res                 \
           d7\JclSIMDViewExpertDLL.res              \
           d7\JclDeveloperTools.res                 \
           d7\JclStackTraceViewerExpert.res         \
           d7\JclStackTraceViewerExpertDLL.res      \
           d7\JclUsesExpert.res                     \
           d7\JclUsesExpertDLL.res                  \
           d7\JclVcl.res                            \
           d7\JclVersionControlExpert.res           \
           d7\JclVersionControlExpertDLL.res        \
           d8\Jcl.res                               \
           d8\JclBaseExpert.res                     \
           d8\JclContainers.res                     \
           d8\JclFavoriteFoldersExpertDLL.res       \
           d8\JclDeveloperTools.res                 \
           d8\JclVersionControlExpertDLL.res        \
           d8\JclVcl.res                            \
           d9\Jcl.res                               \
           d9\JclBaseExpert.res                     \
           d9\JclContainers.res                     \
           d9\JclDebugExpert.res                    \
           d9\JclDebugExpertDLL.res                 \
           d9\JclFavoriteFoldersExpert.res          \
           d9\JclFavoriteFoldersExpertDLL.res       \
           d9\JclProjectAnalysisExpert.res          \
           d9\JclProjectAnalysisExpertDLL.res       \
           d9\JclRepositoryExpert.res               \
           d9\JclRepositoryExpertDLL.res            \
           d9\JclSIMDViewExpert.res                 \
           d9\JclSIMDViewExpertDLL.res              \
           d9\JclDeveloperTools.res                 \
           d9\JclStackTraceViewerExpert.res         \
           d9\JclStackTraceViewerExpertDLL.res      \
           d9\JclVcl.res                            \
           d9\JclVersionControlExpert.res           \
           d9\JclVersionControlExpertDLL.res
# ---------------------------------------------------------------------------
!if !$d(BRCC32)
BRCC32 = brcc32
!endif
# ---------------------------------------------------------------------------
!if $d(PATHRC)
.PATH.res  = $(PATHRC)
!endif
# ---------------------------------------------------------------------------
resources.res: $(RESFILES)

# ---------------------------------------------------------------------------
.rc.res:
    &"$(ROOTDIR)\BIN\$(BRCC32)" -fo$@ $<

# ---------------------------------------------------------------------------




