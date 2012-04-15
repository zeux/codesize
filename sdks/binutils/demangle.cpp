#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <bfd.h>

#define DMGL_NO_OPTS	 0		/* For readability... */
#define DMGL_PARAMS	 (1 << 0)	/* Include function args */
#define DMGL_ANSI	 (1 << 1)	/* Include const, volatile, etc */
#define DMGL_VERBOSE	 (1 << 3)	/* Include implementation details.  */
#define DMGL_TYPES	 (1 << 4)	/* Also try to demangle type encodings.  */
#define DMGL_RET_POSTFIX (1 << 5)       /* Print function return types (when
					   present) after function signature.
					   It applies only to the toplevel
					   function type.  */

int main()
{
    char buf[128*1024];

    while (fgets(buf, sizeof(buf), stdin))
    {
        char* bufe = buf + strlen(buf);
        while (bufe > buf && bufe[-1] <= ' ') *--bufe = 0;

        char* result = bfd_demangle(0, buf, DMGL_PARAMS | DMGL_ANSI | DMGL_VERBOSE | DMGL_RET_POSTFIX);
        printf("%s\n", result ? result : buf);
        free(result);
    }
}
