int main() {
  __asm__("movq rax, 2");
  __asm__("syscall");
  return 0;
}
