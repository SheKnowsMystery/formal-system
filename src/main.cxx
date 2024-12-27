
#include <ios>
#include <iostream>
#include <fstream>
#include <format>

import fs.core;

// функция для открытия файлов,
// которая выбрасывает исключение, если открыть файл не удалось
template <fs::util::any_of<std::fstream, std::ifstream, std::ofstream> _Fstream>
auto open(std::string_view _filepath, std::ios::openmode _mode = 0) -> _Fstream
{
	auto file = _Fstream(_filepath.data(), _mode);
	if (file.fail())
		throw fs::util::failure(std::format(R"(Failed to open file "{}")", _filepath));
	return file;
}

int main()
{
	// во время работы ФС и работы с файлами могут выброситься исключения
	// блок чтобы поймать и обработать эти исключения
	try {
		// данные на вход
		auto in = open<std::ifstream>("in.txt");
		// результаты программы на выход
		auto out = open<std::ofstream>("out.txt", std::ios::trunc);

		// инициализируем ФС
		auto program = fs::Program(in);

		// запускаем вычисления и передаём поток для логирования в выходной файл
#ifdef _DEBUG
		program.process(std::cout);
#else
		program.process(out);
#endif
	}
	catch (const fs::util::failure& failure)
	{ std::cout << "ERROR: " << failure.what() << std::endl; }

	return 0;
}