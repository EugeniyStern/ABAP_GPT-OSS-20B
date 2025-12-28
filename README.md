# Chat Application

Простое веб-приложение чата на SAP UI5, которое позволяет отправлять и отображать сообщения в реальном времени.

## Описание

Приложение представляет собой интерфейс чата с автоматической прокруткой к новым сообщениям и динамическим расчетом высоты контейнера сообщений.

## Технологии

- **SAP UI5** (версия 1.54.0+)
- **HTML5/CSS3**
- **JavaScript**

## Структура проекта

```
gpt_test_b_chat/
├── Component.js          # Главный компонент приложения
├── manifest.json         # Манифест приложения SAP UI5
├── index.html           # Точка входа приложения
├── controller/
│   └── Chat.controller.js    # Контроллер для логики чата
├── view/
│   └── Chat.view.xml         # XML-представление интерфейса чата
├── model/
│   └── models.js             # Модели данных (JSONModel для сообщений)
└── css/
    └── style.css             # Стили приложения
```

## Основные возможности

- ✅ Отображение сообщений в формате FeedListItem
- ✅ Отправка сообщений через поле ввода
- ✅ Автоматическая прокрутка к последнему сообщению
- ✅ Динамическое вычисление высоты контейнера сообщений
- ✅ Поддержка разных типов отправителей (пользователь, система)
- ✅ Разные иконки для разных типов отправителей
- ✅ Отправка сообщения по Ctrl+Enter
- ✅ Автоматическое изменение размера TextArea при вводе

## Запуск
Словарь для класса YCL_LLM_CHAT_HANDLER
YLLM_INTENTS	0001	MANDT	A	0000	X	CLNT		MANDT
YLLM_INTENTS	0002	INTENT_NAME	A	0000	X	CHAR		CHAR30
YLLM_INTENTS	0003	CATEGORY	A	0000		CHAR		CHAR30
YLLM_INTENTS	0004	CLASS_NAME	A	0000		CHAR		CHAR30
YLLM_INTENTS	0005	DESCRIPTION	A	0000		CHAR		TEXT200
YLLM_INTENTS	0006	ACTIVE	A	0000		CHAR		FLAG
YLLM_MESSAGES	0001	MANDT	A	0000	X	CLNT		MANDT
YLLM_MESSAGES	0002	SESSION_ID	A	0000	X	CHAR		CHAR32
YLLM_MESSAGES	0003	MESSAGE_NUM	A	0000	X	INT4		INT4
YLLM_MESSAGES	0004	ROLE	A	0000		CHAR		CHAR20
YLLM_MESSAGES	0005	CONTENT	A	0000		STRG		
YLLM_MESSAGES	0006	TIMESTAMP	A	0000		DEC		TIMESTAMP
YLLM_MESSAGES	0007	TOOL_CALLS	A	0000		STRG		
YLLM_MESSAGES	0008	TOOL_CALL_ID	A	0000		CHAR		CHAR32
YLLM_SESSIONS	0001	MANDT	A	0000	X	CLNT		MANDT
YLLM_SESSIONS	0002	SESSION_ID	A	0000	X	CHAR		CHAR32
YLLM_SESSIONS	0003	START_TIMESTAMP	A	0000		DEC		TIMESTAMP
YLLM_SESSIONS	0004	SAP_LOGON	A	0000		CHAR		USERNAME
YLLM_SESSIONS	0005	NICKNAME	A	0000		CHAR		CHAR30
YLLM_SESSIONS	0006	SESSION_TITLE	A	0000		STRG		
YLLM_USERS	0001	MANDT	A	0000	X	CLNT		MANDT
YLLM_USERS	0002	SAP_LOGON	A	0000	X	CHAR		USERNAME
YLLM_USERS	0003	NICKNAME	A	0000	X	CHAR		CHAR30
YLLM_USERS	0004	LLM_NAME	A	0000		CHAR		TEXT60
YLLM_USERS	0005	GENDER	A	0000		CHAR		CHAR1
YLLM_USERS	0006	LANGUAGE_PREF	A	0000		LANG		LANG



1. Откройте `index.html` в браузере
2. Или используйте локальный веб-сервер (например, через Python: `python -m http.server 8080`)

## Использование

- Введите сообщение в поле ввода внизу экрана
- Нажмите кнопку отправки или используйте **Ctrl+Enter**
- Сообщения автоматически прокручиваются вниз
- Высота контейнера сообщений автоматически подстраивается под размер окна

## Особенности реализации

- **Динамическая высота**: ScrollContainer автоматически рассчитывает свою высоту как разницу между доступной высотой экрана и высотой панели ввода
- **Автопрокрутка**: При добавлении нового сообщения список автоматически прокручивается к последнему элементу
- **Адаптивность**: Приложение адаптируется к изменению размера окна браузера

## Лицензия

MIT

