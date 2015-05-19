#if ! defined(_DEBUG_H_)
#     define  _DEBUG_H_

#if defined(__cplusplus)
extern "C" {
  int doDebug (void);
}
#else
int doDebug (void);
#endif

#endif /* #if ! defined(_DEBUG_H_) */
