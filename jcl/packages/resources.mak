ROOTDIR = $(MAKEDIR)\..

# ---------------------------------------------------------------------------
RESFILES = c5\JclBaseExpertC50.res                  \
           c5\JclThreadNameExpertC50.res            \
           c5\JclC50.res                            \
           c5\JclDebugExpertC50.res                 \
           c5\JclFavoriteFoldersExpertC50.res       \
           c5\JclDebugExpertDLLC50.res              \
           c5\JclFavoriteFoldersExpertDLLC50.res    \
           c5\JclProjectAnalysisExpertC50.res       \
           c5\JclProjectAnalysisExpertDLLC50.res    \
           c5\JclSIMDViewExpertC50.res              \
           c5\JclThreadNameExpertDLLC50.res         \
           c5\JclSIMDViewExpertDLLC50.res           \
           c5\JclUsesExpertC50.res                  \
           c5\JclUsesExpertDLLC50.res               \
           c5\JclVersionControlExpertC50.res        \
           c5\JclVersionControlExpertDLLC50.res     \
           c6\Jcl.res                               \
           c6\JclBaseExpert.res                     \
           c6\JclDebugExpert.res                    \
           c6\JclDebugExpertDLL.res                 \
           c6\JclFavoriteFoldersExpert.res          \
           c6\JclFavoriteFoldersExpertDLL.res       \
           c6\JclProjectAnalysisExpert.res          \
           c6\JclProjectAnalysisExpertDLL.res       \
           c6\JclSIMDViewExpert.res                 \
           c6\JclThreadNameExpertDLL.res            \
           c6\JclSIMDViewExpertDLL.res              \
           c6\JclUsesExpert.res                     \
           c6\JclThreadNameExpert.res               \
           c6\JclUsesExpertDLL.res                  \
           c6\JclVersionControlExpert.res           \
           c6\JclVClx.res                           \
           c6\JclVcl.res                            \
           c6\JclVersionControlExpertDLL.res        \
           cs1\Jcl.res                              \
           cs1\JclBaseExpert.res                    \
           cs1\JclFavoriteFoldersExpertDLL.res      \
           cs1\JclVersionControlExpertDLL.res       \
           d10\JclUsesExpert.res                    \
           d10\Jcl.res                              \
           d10\JclBaseExpert.res                    \
           d10\JclDebugExpert.res                   \
           d10\JclDebugExpertDLL.res                \
           d10\JclFavoriteFoldersExpert.res         \
           d10\JclProjectAnalysisExpert.res         \
           d10\JclFavoriteFoldersExpertDLL.res      \
           d10\JclProjectAnalysisExpertDLL.res      \
           d10\JclSIMDViewExpert.res                \
           d10\JclUsesExpertDLL.res                 \
           d10\JclSIMDViewExpertDLL.res             \
           d10\JclVcl.res                           \
           d10\JclThreadNameExpert.res              \
           d10\JclThreadNameExpertDLL.res           \
           d10\JclVersionControlExpert.res          \
           d10\JclVersionControlExpertDLL.res       \
           d5\JclUsesExpertDLLD50.res               \
           d5\JclBaseExpertD50.res                  \
           d5\JclD50.res                            \
           d5\JclDebugExpertD50.res                 \
           d5\JclProjectAnalysisExpertDLLD50.res    \
           d5\JclDebugExpertDLLD50.res              \
           d5\JclFavoriteFoldersExpertD50.res       \
           d5\JclFavoriteFoldersExpertDLLD50.res    \
           d5\JclProjectAnalysisExpertD50.res       \
           d5\JclSIMDViewExpertD50.res              \
           d5\JclSIMDViewExpertDLLD50.res           \
           d5\JclThreadNameExpertD50.res            \
           d5\JclThreadNameExpertDLLD50.res         \
           d5\JclUsesExpertD50.res                  \
           d5\JclVersionControlExpertD50.res        \
           d5\JclVersionControlExpertDLLD50.res     \
           d5.dev\JclD50.res                        \
           d6\Jcl.res                               \
           d6\JclBaseExpert.res                     \
           d6\JclDebugExpert.res                    \
           d6\JclDebugExpertDLL.res                 \
           d6\JclFavoriteFoldersExpert.res          \
           d6\JclFavoriteFoldersExpertDLL.res       \
           d6\JclProjectAnalysisExpert.res          \
           d6\JclProjectAnalysisExpertDLL.res       \
           d6\JclSIMDViewExpert.res                 \
           d6\JclVcl.res                            \
           d6\JclSIMDViewExpertDLL.res              \
           d6\JclThreadNameExpert.res               \
           d6\JclThreadNameExpertDLL.res            \
           d6\JclUsesExpert.res                     \
           d6\JclUsesExpertDLL.res                  \
           d6\JclVClx.res                           \
           d6\JclVersionControlExpert.res           \
           d6\JclVersionControlExpertDLL.res        \
           d6.dev\JclVcl.res                        \
           d7\Jcl.res                               \
           d7\JclBaseExpert.res                     \
           d7\JclDebugExpert.res                    \
           d7\JclDebugExpertDLL.res                 \
           d7\JclFavoriteFoldersExpert.res          \
           d7\JclFavoriteFoldersExpertDLL.res       \
           d7\JclProjectAnalysisExpert.res          \
           d7\JclProjectAnalysisExpertDLL.res       \
           d7\JclSIMDViewExpert.res                 \
           d7\JclVcl.res                            \
           d7\JclSIMDViewExpertDLL.res              \
           d7\JclThreadNameExpert.res               \
           d7\JclThreadNameExpertDLL.res            \
           d7\JclUsesExpert.res                     \
           d7\JclUsesExpertDLL.res                  \
           d7\JclVClx.res                           \
           d7\JclVersionControlExpert.res           \
           d7\JclVersionControlExpertDLL.res        \
           d7.dev\JclVClx.res                       \
           d7.dev\JclVcl.res                        \
           d8\Jcl.res                               \
           d8\JclBaseExpert.res                     \
           d8\JclFavoriteFoldersExpertDLL.res       \
           d8\JclVersionControlExpertDLL.res        \
           d9\JclThreadNameExpertDLL.res            \
           d9\Jcl.res                               \
           d9\JclBaseExpert.res                     \
           d9\JclDebugExpert.res                    \
           d9\JclDebugExpertDLL.res                 \
           d9\JclFavoriteFoldersExpert.res          \
           d9\JclFavoriteFoldersExpertDLL.res       \
           d9\JclProjectAnalysisExpert.res          \
           d9\JclProjectAnalysisExpertDLL.res       \
           d9\JclSIMDViewExpert.res                 \
           d9\JclUsesExpert.res                     \
           d9\JclSIMDViewExpertDLL.res              \
           d9\JclUsesExpertDLL.res                  \
           d9\JclThreadNameExpert.res               \
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




