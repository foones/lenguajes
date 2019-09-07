#include "Fu.h"

#ifndef CLOCK_TICK_RATE
#define CLOCK_TICK_RATE 1193180
#endif

#ifdef WIN32

#include <windows.h>
#include <conio.h>

#include "modsound.h"

int fd = -1;

void sound(float freq, unsigned int duration)
{
	unsigned int i;
	/* Win9X */

	int speaker_state;

	/* Force timer into oscillator mode via timer control port. */
	_outp(0x43, 0xb6);

	/* Compute ratio of ancient hardcoded timer frequency to
	 * frequency we want.  Then feed that ratio (lowest byte
	 * first) into timer data port.
	 */
	int ifreq = (int)(CLOCK_TICK_RATE / freq);

	_outp(0x42, ifreq & 0xff);
	_outp(0x42, (ifreq >> 8) & 0xff);
	/* Get speaker control state. */
	speaker_state = _inp(0x61);
	/* Turn the speaker on (bit 1)
	 * and drive speaker from timer (bit 0).
	 */
	_outp(0x61, speaker_state | 0x3);
	/* Let it blast in peace for the duration. */
	Sleep(duration);
	/* Restore speaker control to original state. */
	_outp(0x61, speaker_state);
}

#else /* WIN32 */

#include <fcntl.h>
#include <linux/kd.h>

#include "modsound.h"

int fd = -1;

void sound(float freq, unsigned int duration)
{
	if ((fd = open(ctermid(NULL), O_WRONLY)) == -1) return;

	if (freq > 0 && ioctl(fd, KIOCSOUND, (int)(CLOCK_TICK_RATE/(float) freq)) < 0) return;
	usleep(1000*duration);

	ioctl(fd, KIOCSOUND, 0);	
	close(fd); fd = -1;
}

#endif	/* WIN32 */
