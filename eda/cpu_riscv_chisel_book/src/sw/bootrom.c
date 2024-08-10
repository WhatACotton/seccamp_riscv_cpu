#include <stdint.h>
#include <stdbool.h>

extern void __attribute__((naked)) __attribute__((section(".isr_vector"))) isr_vector(void)
{
    asm volatile ("j start");              // Reset
    asm volatile ("j start");              // S-mode software interrupt
    asm volatile ("j start");              // VS-mode software interrupt
    asm volatile ("j start");              // M-mode software interrupt
    asm volatile ("j start");              // Reserved
    asm volatile ("j start");              // S-mode timer interrupt
    asm volatile ("j start");              // VS-mode timer interrupt
    asm volatile ("j start");              // M-mode timer interrupt
    asm volatile ("j start");              // Reserved
    asm volatile ("j start");              // S-mode external interrupt
    asm volatile ("j start");              // VS-mode external interrupt
    asm volatile ("j start");              // M-mode external interrupt
}

void __attribute__((noreturn)) main(void);
extern uint32_t _bss_start;
extern uint32_t _bss_end;
// extern uint32_t _data_start;
// extern uint32_t _data_end;
// extern uint32_t _data_rom_start;
void init(void)
{
    uint32_t* bss_end = &_bss_end; 
    for(volatile uint32_t* bss = &_bss_start; bss < bss_end; bss++) {
        *bss = 0;
    }
    // uint32_t* data_end = &_data_end; 
    // volatile uint32_t* data_rom = &_data_rom_start; 
    // for(volatile uint32_t* data = &_data_start; data < data_end; data++, data_rom++) {
    //     *data = *data_rom;
    // }
}

extern void __attribute__((naked)) start(void)
{
    asm volatile ("la sp, ramend");
    asm volatile ("addi sp, sp, -4");
    init();
    main();
}

static volatile uint32_t* const REG_GPIO_LED =    (volatile uint32_t*)0xA0000000;

void __attribute__((noreturn)) main(void)
{
    uint32_t counter = 0;
    uint32_t led_out = 1;
    const LED_BITS = 6;

    for(;;) {
        if( (counter & 0xffff) == 0xffff ) {
            // Update LEDs
            led_out = (led_out << 1) | (led_out >> (LED_BITS - 1));
        }
        *REG_GPIO_LED = led_out;
        counter++;
    }
}
