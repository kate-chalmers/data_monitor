
// Bounce function

const BOUNCE_DURATION_MS = 4000; // 3x 1s animation
const INTERVAL_MS = 25000;       // repeat cadence
const FIRST_DELAY_MS = 1000;     // quick first bounce for visibility

function triggerBounce() {
  // Find the single label we want to show (the one that's not 'off')
    const label = document.querySelector('.bounce-label:not(.bounce-label-off)');
    if (!label) return;

    // Bounce the card that actually contains that label
    const card = label.closest('.card');
    if (!card) return;

    card.classList.add('bounce');
    label.classList.add('show');

    setTimeout(() => {
      card.classList.remove('bounce');
      label.classList.remove('show');
    }, BOUNCE_DURATION_MS);
  }

  // Start after DOM is ready
  document.addEventListener('DOMContentLoaded', function() {
    setTimeout(triggerBounce, FIRST_DELAY_MS);
    setInterval(triggerBounce, INTERVAL_MS);
  });