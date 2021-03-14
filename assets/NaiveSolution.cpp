#include <filesystem>
#include <fstream>
#include <cstring>
#include <string>

#ifndef CHECH_ERR
#define CHECK_ERR(intro, cond)                      \
do {                                                \
    if (cond)                                       \
    {                                               \
        perror(intro);                              \
        return -1;                                  \
    }                                               \
} while (false)
#endif


int main(int argc, char** argv)
{
    if (argc != 3)
    {
        fputs("usage: findstr FNAME STR", stderr);
        return -1;
    }

    FILE* file = fopen(argv[1], "r");
    CHECK_ERR("Open failed", file == nullptr);

    constexpr size_t buff_sz = 4096 * 1024;
    std::string buffer(buff_sz, '\0');

    size_t rd = fread(buffer.data(), 1, buff_sz, file);
    CHECK_ERR("Read failed", ferror(file));
    fclose(file);

    buffer[rd] = '\0';

    auto result = buffer.find(argv[2]) != buffer.npos;
    puts(result ? "Yes" : "No");
}

