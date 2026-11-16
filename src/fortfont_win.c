/* UTF-8 font discovery over the Windows wide-character filesystem API. */
#ifdef _WIN32
#include <windows.h>
#include <wchar.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
typedef struct { HANDLE h; WIN32_FIND_DATAW data; int first; char name[4096]; } listing;
static wchar_t *wide(const char *s) {
    int n=MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,s,-1,NULL,0);
    wchar_t *w=n?malloc((size_t)n*sizeof *w):NULL;
    if(w)MultiByteToWideChar(CP_UTF8,0,s,-1,w,n);
    return w;
}
void *fortfont_opendir(const char *path) {
    wchar_t *w=wide(path);if(!w)return NULL;
    DWORD attr=GetFileAttributesW(w);free(w);
    if(attr==INVALID_FILE_ATTRIBUTES || !(attr&FILE_ATTRIBUTE_DIRECTORY))return NULL;
    size_t n=strlen(path);char *pattern=malloc(n+3);if(!pattern)return NULL;
    snprintf(pattern,n+3,"%s/*",path);w=wide(pattern);free(pattern);if(!w)return NULL;
    listing *d=calloc(1,sizeof *d);if(!d){free(w);return NULL;}
    d->h=FindFirstFileW(w,&d->data);free(w);d->first=1;
    if(d->h==INVALID_HANDLE_VALUE){free(d);return NULL;}return d;
}
void *fortfont_readdir(void *p) {
    listing *d=p;if(!d->first && !FindNextFileW(d->h,&d->data))return NULL;
    d->first=0;
    if(!WideCharToMultiByte(CP_UTF8,0,d->data.cFileName,-1,d->name,sizeof d->name,NULL,NULL))return NULL;
    return d->name;
}
int fortfont_closedir(void *p){listing*d=p;int rc=FindClose(d->h)?0:-1;free(d);return rc;}
int fortfont_mkdir(const char *path,int mode){(void)mode;wchar_t*w=wide(path);if(!w)return -1;int rc=CreateDirectoryW(w,NULL)?0:-1;free(w);return rc;}
#endif
