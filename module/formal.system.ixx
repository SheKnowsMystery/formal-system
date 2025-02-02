module;

#include <type_traits>
#include <concepts>
#include <cstddef>

#include <stdexcept>
#include <format>

#include <string_view>
#include <string>
#include <regex>

#include <istream>
#include <iomanip>

#include <functional>
#include <algorithm>
#include <numeric>

#include <iterator>
#include <ranges>
#include <vector>
#include <set>
#include <map>

export module fs.core;

// пространство имён с утилитами
export
namespace fs::util {
	// концепт для проверки правильности типа
	template <typename _Type>
	concept expandable_container =
		std::ranges::range<_Type> &&
		requires(_Type _obj, std::ranges::range_value_t<_Type> _val) { _obj.insert(_obj.end(), _val); };
	// концепт для проверки правильности типа
	template <typename _Container, typename _Type>
	concept contains = std::same_as<std::ranges::range_value_t<_Container>, _Type>;
	// концепт для проверки правильности типа
	template <typename _Type, typename... _Types>
	concept any_of = (std::same_as<_Type, _Types> || ...);
	// концепт для проверки правильности типа
	template <typename _Type, typename... _Types>
	concept constructible_from_any_of = (std::constructible_from<_Type, _Types> || ...);
	// функция для получения строки из потока
	auto getline(std::istream& _is, char _delim = '\n') -> std::string {
		std::string line;
		std::getline(_is, line, _delim);
		return line;
	}
	// класс исключения нашей программы
	class failure : public std::runtime_error {
		using Base = std::runtime_error;
	public:
		using Base::Base;
		failure(std::string_view _msg, std::string_view _text, std::size_t _index) :
			Base(std::format("{0}\n{1}\n{3:>{2}}", _msg, _text, _index + 1, '^')) {};
		failure(std::string_view _msg, std::string_view _text) :
			Base(std::format("{0}\n{1}\n{3:^>{2}}", _msg, _text, _text.size(), '^')) {};
		failure(std::string_view _msg, char _text) :
			Base(std::format("{0}\n{1}\n^", _msg, _text)) {};
	};
}

// пространство имён с имплементацией
namespace fs::impl {
	// структура, содержащая информацию о контейнере, который хранит обычные значения
	template <typename _Type>
	struct Pass {
		using type = std::remove_cvref_t<_Type>;
		static constexpr auto call = std::identity();
	};
	// структура, содержащая информацию о контейнере, который хранит пары значений
	template <typename _Key, typename _Value>
	struct Pass<std::pair<_Key, _Value>> {
		using type = std::remove_cvref_t<_Key>;
		template <typename _Type>
		static auto call(_Type&& _val) -> std::pair<_Key, _Value>
		{ return { std::forward<_Type>(_val), {} }; }
	};

	// пространство имён с регулярными выражения для парсинга строк программы
	namespace prs {
		const auto regex = std::regex(R"(\s*\w+\s*=\s*\{([^}]*)\}\s*)");
		const auto split = std::regex(R"(\s*([^,\s]+)\s*)");
	}

	// функция которая возвращает контейнер, заполненный в результатами парсинга
	template <util::expandable_container _Container>
		requires(!std::same_as<_Container, std::string>)
	auto parse(const std::string& _raw) {
		// контейнер, в который будет помещаться результат парсинга
		auto container = _Container();
		// захват-группа для отдельных элементов
		auto capture = std::smatch();
		// проверяем подходит ли входная строка под регулярное выражение
		if (std::regex_search(_raw, capture, prs::regex)) {
			// извлекаем из строки то, что находится между фигурными скобками
			auto contents = capture[1].str();
			// создаём итераторы регулярных выражений чтобы разделить полученную строку на элементы
			auto begin = std::sregex_iterator(contents.begin(), contents.end(), prs::split);
			auto end   = std::sregex_iterator();
			// лямбда для преобразования полученных элементов в строки
			auto transform = [](const std::sregex_iterator::value_type& _value) -> std::string { return _value[1].str(); };
			// создаём диапазон который будет состоять из отдельных элементов, захваченных регулярным выражением
			auto elems = std::ranges::subrange(begin, end) | std::views::transform(transform);
			// узнаём информацию о контейнере
			using pass = Pass<std::ranges::range_value_t<_Container>>;
			// получаем тип, который хранит контейнер, в который нам нужно будет привести полученные элементы
			using type = typename pass::type;
			// тип контейнера должен быть строкой или символом, или быть создаваемым из них
			static_assert(util::any_of<type, std::string, char> || util::constructible_from_any_of<type, std::string, char>);
			// вставляем в контейнер элемент, возвращая его в том виде, в котором он может быть вставлен
			for (const auto& elem : elems) {
				if constexpr (std::same_as<type, std::string> || std::constructible_from<type, std::string>)
					container.insert(container.end(), pass::call(elem));
				if constexpr (std::same_as<type, char> || std::constructible_from<type, char>)
					container.insert(container.end(), pass::call(elem[0]));
			}
		}
		// возвращаем контейнер
		return container;
	}
	// функция которая возвращает строку, полученную в результате парсинга
	template <std::same_as<std::string>>
	auto parse(const std::string& _raw) -> std::string {
		// строка которую мы хотим спарсить из входной строки
		auto str = std::string();
		// захват-группа для строки
		auto capture = std::smatch();
		// ищем нужную часть с помощью регулярных выражений
		if (std::regex_search(_raw, capture, prs::regex)) {
			auto contents = capture[1].str();
			if (std::regex_search(contents, capture, prs::split))
				str = capture[1].str();
		}
		// возвращаем результат
		return str;
	}
	// функция, которая парсит строку по разделителю ';' и возвращает заполненный результатом контейнер
	template <util::expandable_container _Container>
	auto parse(std::istream& _is) -> _Container
	{ return parse<_Container>(util::getline(_is, ';')); }
}

// пространство имён с реализацией формальной системы
export
namespace fs {
	// класс ФС
	class Program {
		struct Rule;
		// синонимы типов для удобной работы
		using symbol_t    = char;
		using alphabet_t  = std::set<symbol_t>;
		using word_t      = std::basic_string<symbol_t>;
		using variable_t  = symbol_t;
		using variables_t = std::map<variable_t, word_t>;
		using axioms_t    = std::set<symbol_t>;
		using rule_t      = Rule;
		using rules_t     = std::vector<rule_t>;
		// структура для хранения правил в удобном формате
		struct Rule {
			word_t raw; // сырая строка правила
			// части правила
			struct Part {
				word_t str; // строка, содержащая регулярное выражение
				word_t raw; // сырая строка части
				std::regex regex; // регулярное выражение части
			} condition, replacement;
			std::size_t varc = 0;
			std::map<std::size_t, variable_t> variables; // список
			// конструктор правила из сырой строки
			Rule(const word_t& _raw) : raw(_raw) {
				// регулярное выражение, которое определяет вид правила
				static const auto regex = std::regex(R"((\S*)->(\S*))");
				// захват-группа для частей правила
				auto capture = std::smatch();
				// проверяем, подходит ли правило под регулярное выражение, и присваиваем частям правила их строки, иначе выбрасываем исключение
				if (std::regex_search(_raw, capture, regex) && capture.size() == 3) {
					condition.str   = escape(condition.raw   = capture[1].str());
					replacement.str = escape(replacement.raw = capture[2].str());
				}
				else throw util::failure("Invalid rule structure", raw);
			}
			// функция, добавляющая перед всеми специальными символами '\', чтобы они не влияли на регулярное выражение
			static auto escape(const word_t& _raw) -> word_t {
				static const auto special = std::regex(R"(\W)");
				return std::regex_replace(_raw, special, "\\$&");
			}
			// функция, возвращающая индекс переменной в части
			static auto find(Part& _part, variable_t _var) -> std::size_t {
				auto var      = std::regex(std::string() + _var);
				auto capture = std::smatch();
				if(std::regex_search(_part.raw, capture, var))
					return std::distance(_part.raw.cbegin(), capture[1].first);
				return 0;
			}
			// функция, которая создаёт регулярное выражение для части правила
			static bool set(Part& _part, variable_t _var, std::size_t _id, const word_t& _axiom_str) {
				// получаем, сколько переменных уже заменено в части правила
				std::size_t varc = _part.regex.mark_count();
				// копируем часть правила в новую строку
				auto result = std::string(_part.str);
				// создаём  регулярное выражение для переменной
				auto var = std::regex(std::string() + _var);
				// заменяем первую переменную на регулярное выражение списка аксиом
				result = std::regex_replace(result, var, _axiom_str, std::regex_constants::format_first_only);
				// повторы переменной заменяем на индекс переменной, чтобы при поиске регулярного выражения захват-группа была одинаковой для одинаковых переменных
				result = std::regex_replace(result, var, '\\' + std::to_string(_id));
				// присваиваем полученное регулярное выражение
				_part.regex = _part.str = result;
				// возвращаем были ли замены переменных или нет
				return varc < _part.regex.mark_count();
			}
			// функция установки регулярного выражения для правила, с помощью подстановки списка аксиом вместо переменных
			void set(variable_t _var, const word_t& _axiom_str) {
				// вспомогательная структура, которая отслеживает, где в правиле была найдена переменная
				struct {
					bool condition   = false;
					bool replacement = false;
				} add;
				std::size_t id = varc + 1;
				// создаём регулярные выражения
				add.condition   |= set(condition, _var, id, _axiom_str);
				add.replacement |= set(replacement, _var, id, _axiom_str);
				// если переменная найдена в замене, но не найдена в условии - правило некорректно
				if (!add.condition && add.replacement)
					throw util::failure("Invalid replacement in the rule", raw, condition.raw.size() + 1 + find(replacement, _var));
				// если переменная найдена в условии, добавляем её в список переменных этого правила
				if (!add.condition)
					return;
				variables[id] = _var;
				varc++;
			}
		};
		// данные программы
		alphabet_t  alphabet;
		variables_t variables;
		axioms_t    axioms;
		rules_t     rules;
		// вычисляемое выражение
		word_t expression;
		// функция, возвращающая лямбду для проверки символа на наличие в алфавите
		auto correct() const
		{ return [this](symbol_t _sym) -> bool { return alphabet.contains(_sym); }; }
		// функция, возвращающая лямбду для проверки символа на наличие в алфавите и списке переменных
		auto correct_v() const
		{ return [this](symbol_t _sym) -> bool { return alphabet.contains(_sym) || variables.contains(_sym); }; }
		// функция для проверки символа на наличие в алфавите
		auto correct(symbol_t _sym) const
		{ return alphabet.contains(_sym); }
		// функция для проверки символа на наличие в алфавите и списке переменных
		auto correct_v(symbol_t _sym) const
		{ return alphabet.contains(_sym) || variables.contains(_sym); }
		// функция верификации выражения для проверки наличия всех символов выражения в алфавите
		bool correct(const word_t& _word) const
		{ return std::ranges::all_of(_word, correct()); }
		// функция верификации выражения для проверки наличия всех символов выражения в алфавите и списке переменных
		bool correct_v(const word_t& _word) const
		{ return std::ranges::all_of(_word, correct_v()); }
		// функция, которая возвращает индекс первого некорректного символа в правиле
		auto incorrect(const rule_t& _rule) const -> std::size_t {
			auto condition   = std::ranges::find_if_not(_rule.condition.raw, correct_v());
			if (_rule.condition.raw.end() != condition)
				return std::distance(_rule.condition.raw.begin(), condition);
			auto replacement = std::ranges::find_if_not(_rule.replacement.raw, correct_v());
			if (_rule.replacement.raw.end() != replacement)
				return std::distance(_rule.replacement.raw.begin(), replacement) + _rule.condition.raw.size() + 2;
			return 0;
		}
		// функция, которая возвращает индекс первого некорректного символа в правиле
		auto incorrect(const word_t& _word) const -> std::size_t {
			auto iter = std::ranges::find_if_not(_word, correct());
			return std::distance(_word.begin(), iter);
		}
		// функция верификации правил, которая проверяет, все ли символы правила содержатся в алфавите или в списке переменных
		bool verify(const rule_t& _rule) const {
			auto correct = [this](symbol_t _sym) -> bool { return alphabet.contains(_sym) || variables.contains(_sym); };
			return std::ranges::all_of(_rule.condition.raw, correct) && std::ranges::all_of(_rule.replacement.raw, correct);
		}
		// функция верификации выражений, которая проверяет, все ли символы выражения содержатся в алфавите
		bool verify(const word_t& _expression) const {
			auto correct = [this](symbol_t _sym) -> bool { return alphabet.contains(_sym); };
			return std::ranges::all_of(_expression, correct);
		}
		// функция, проверяющая данные и инициализирующая необходимые части ФС
		void prepare() {
			// проверяем правила на корректность
			for (const auto& rule : rules)
				if (!verify(rule))
					throw util::failure("Invalid symbol in the rule", rule.raw, incorrect(rule));
			// проверяем аксиомы на корректность
			for (const auto& axiom : axioms)
				if (!correct(axiom))
					throw util::failure("Invalid symbol in the axiom list", axiom);
			// создаём регулярное выражение для аксиом
			auto axiom_str = word_t();
			axiom_str = std::accumulate(axioms.begin(), axioms.end(), word_t());
			axiom_str = "([" + axiom_str + "]+)";
			// создаём регулярные выражения для правил
			for (auto& [variable, value] : variables)
				for (auto& rule : rules)
					rule.set(variable, axiom_str);
			// проверяем выражение на корректность
			if (!verify(expression))
				throw util::failure("Invalid symbol in the expression", expression, incorrect(expression));
		}
		// функция поиска правила, подходящего к выражению
		auto find(const word_t& _expression) const -> const rule_t* {
			// создаём пустой указатель на правило, который останется нулевой, если мы не найдём правило
			const rule_t* result = nullptr;
			// итерируемся по правилам, ища совпадения с помощью регулярных выражений
			for (const auto& rule : rules)
				if (std::regex_search(_expression, rule.condition.regex))
					return result = &rule;
			// возвращаем найденное правило или пустой указатель
			return result;
		}
		// функция замены в выражении по найденному правилу
		auto replace(const word_t& _expression, const rule_t* _rule) -> word_t {
			// создаём переменную, которая будет хранить результат
			auto result = word_t();
			// на всякий случай проверяем, передано ли правило
			if (!_rule)
				return result;
			// создаём захват-группу, в которой будут хранится найденные переменные
			auto capture = std::smatch();
			// ищем подходящую под правило часть выражения
			if (std::regex_search(_expression, capture, _rule->condition.regex)) {
				// на всякий случай проверяем, все ли переменные найдены
				// необязательная проверка [[deprecated]]
				if (capture.size() - 1 != _rule->variables.size())
					throw;
				// присваиваем переменным в ФС найденные значения по их именам
				for (auto [id, var] : _rule->variables)
					variables.at(var) = capture[id];
				// получаем шаблон строки, на который будем производить замену
				auto replacement = _rule->replacement.raw;
				// производим замену переменных в шаблоне на переменные, которые мы сохранили ранее
				for (auto [id, var] : _rule->variables)
					replacement = std::regex_replace(replacement, std::regex(std::string() + var), variables.at(var));
				// производим замену в выражении
				result = std::regex_replace(_expression, _rule->condition.regex, replacement);
				// обнуляем переменные
				for (auto [id, var] : _rule->variables)
					variables.at(var) = {};
			}
			// возвращаем результат
			return result;
		}
		// функция логирования
		static void log(std::ostream& _os, const word_t& _expr, const rule_t* _rule = nullptr) {
			_os << '<' << std::setw(_expr.size() + 6) << std::setfill('-') << '|' << std::endl;
			_os << "EXPR: " << _expr << std::endl;
			_os << "RULE: " << (_rule ? _rule->raw : "NOT_FOUND") << std::endl;
		}
	public:
		Program(std::istream& _data) :
		// инициализация ФС путём парсинга потока
			alphabet  (impl::parse<alphabet_t> (_data)),
			variables (impl::parse<variables_t>(_data)),
			axioms    (impl::parse<axioms_t>   (_data)),
			rules     (impl::parse<rules_t>    (_data)),
			expression(impl::parse<word_t>     (_data))
		// подготавливаем данные ФС к вычислениям
		{ prepare(); }
		// функция выполнения ФС
		void process(std::ostream& _out) {
			const rule_t* rule = nullptr;
			// цикл вычислений продолжается, если правило найдено
			while ((rule = find(expression))) {
				// записываем информацию о цикле в предоставленный поток
				// - текущее выражение
				// - правило, которое будет применено
				log(_out, expression, rule);
				// выполняем замену в выражении по найденному правилу
				expression = replace(expression, rule);
			}
			// записываем финальное состояние выражения в предоставленный поток
			log(_out, expression, rule);
		}
	};
}