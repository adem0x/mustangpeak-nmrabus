
{.$DEFINE SIMULATE_SPI}                 // Skips over hardware loops in the SPI Code

{$DEFINE DCCTIMER_DISABLE}             // Does not enable the DCC 56us Timer


{.$DEFINE WATCHDOGTIMER_DISABLE}         // Does not enable the DCC 100ms Timer

{$DEFINE UART_ENABLE}                  // Adds/Removes all UART Code from the build

{.$DEFINE PINCHANGE_INTERRUPT_ENABLE}   // Enables the Pin Change Interrupt

{$DEFINE SPI_ENABLE}                   // Enables the SPI Module

{$DEFINE ENABLE_TRAPS}                 // Enables the Trap Interrupts on the MCU, uses the dsPICxx_Traps.mpas file
{.$DEFINE ENABLE_TOGGLE_PIN_ON_TRAP}    // Enables the Trap Interrupts on the MCU, uses the dsPICxx_Traps.mpas file
{$DEFINE ENABLE_LOOP_FOREVER_ON_TRAP}  // Enables the Trap Interrupts on the MCU, uses the dsPICxx_Traps.mpas file

{$DEFINE FIX_NESTED_INTERRUPT_SILICON_BUG}    // Add code to fix a silicon bug in nested Interrupts

{$DEFINE SUPPORT_AT_LEAST_ONE_CONSUMED_EVENT}
{$DEFINE SUPPORT_AT_LEAST_ONE_PRODUCED_EVENT}

{$DEFINE SUPPORT_AT_LEAST_ONE_VNODE_CONSUMED_EVENT}
{$DEFINE SUPPORT_AT_LEAST_ONE_VNODE_PRODUCED_EVENT}

{$DEFINE SUPPORT_VIRTUAL_NODES}

